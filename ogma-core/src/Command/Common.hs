{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- Copyright 2022 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY
-- OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT
-- LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
-- SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
-- PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE
-- SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF
-- PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN
-- ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR
-- RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR
-- ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE. FURTHER,
-- GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING
-- THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES
-- IT "AS IS."
--
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST
-- THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS
-- ANY PRIOR RECIPIENT. IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN
-- ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE,
-- INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S
-- USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE
-- UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
-- PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW. RECIPIENT'S SOLE REMEDY
-- FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS
-- AGREEMENT.
--
-- | Shared functions across multiple backends.
module Command.Common
    ( parseInputFile
    , parseVariablesFile
    , parseRequirementsListFile
    , parseVarDBFile
    , parseVarDBFile'
    , openVarDBFiles
    , parseTemplateVarsFile
    , checkArguments
    , specExtractExternalVariables
    , specExtractHandlers
    , ExprPair(..)
    , ExprPairT(..)
    , exprPair
    , ErrorTriplet(..)
    , ErrorCode
    , processResult
    , cannotCopyTemplate
    , makeLeftE
    , mergeObjects
    , locateTemplateDir
    , VariableDB(..)
    , InputDef(..)
    , Connection(..)
    , TopicDef(..)
    , TypeDef(..)
    , OutputDef(..)
    , findInput
    , findConnection
    , findTopic
    , findType
    )
  where

-- External imports
import qualified Control.Exception      as E
import           Control.Monad.Except   (ExceptT (..), runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON(..), Value (Null, Object),
                                         eitherDecode, eitherDecodeFileStrict,
                                         object, (.:))
import           Data.Aeson.KeyMap      (union)
import           Data.Aeson.Types       (prependFailure, typeMismatch)
import           Data.List              (find, isInfixOf, isPrefixOf)
import           GHC.Generics           (Generic)
import           System.Directory       (doesFileExist)
import           System.FilePath        ((</>))
import           System.Process         (readProcess)

-- External imports: auxiliary
import Data.ByteString.Extra as B (safeReadFile)
import Data.String.Extra     (sanitizeLCIdentifier, sanitizeUCIdentifier)

-- External imports: ogma
import Data.OgmaSpec            (Spec, externalVariableName, externalVariables,
                                 requirementName, requirements)
import Language.JSONSpec.Parser (parseJSONSpec)
import Language.XMLSpec.Parser  (parseXMLSpec)

-- External imports: language ASTs, transformers
import qualified Language.CoCoSpec.AbsCoCoSpec as CoCoSpec
import qualified Language.CoCoSpec.ParCoCoSpec as CoCoSpec ( myLexer,
                                                             pBoolSpec )

import qualified Language.SMV.AbsSMV       as SMV
import qualified Language.SMV.ParSMV       as SMV (myLexer, pBoolSpec)
import           Language.SMV.Substitution (substituteBoolExpr)

import qualified Language.Trans.CoCoSpec2Copilot as CoCoSpec (boolSpec2Copilot,
                                                              boolSpecNames)
import           Language.Trans.SMV2Copilot      as SMV (boolSpec2Copilot,
                                                         boolSpecNames)

-- Internal imports: auxiliary
import Command.Result  (Result (..))
import Data.Location   (Location (..))
import Paths_ogma_core (getDataDir)

-- | Process input specification, if available, and return its abstract
-- representation.
parseInputFile :: FilePath
               -> String
               -> String
               -> Maybe String
               -> ExprPairT a
               -> ExceptT ErrorTriplet IO (Spec a)
parseInputFile fp formatName propFormatName propVia exprT =
  ExceptT $ do
    let ExprPairT parse replace print ids def = exprT

    let wrapper = wrapVia propVia parse
    -- Obtain format file.
    --
    -- A format name that exists as a file in the disk always takes preference
    -- over a file format included with Ogma. A file format with a forward
    -- slash in the name is always assumed to be a user-provided filename.
    -- Regardless of whether the file is user-provided or known to Ogma, we
    -- check (again) whether the file exists, and print an error message if
    -- not.
    exists  <- doesFileExist formatName
    dataDir <- getDataDir
    let formatFile
          | isInfixOf "/" formatName || exists
          = formatName
          | otherwise
          = dataDir </> "data" </> "formats" </>
               (formatName ++ "_" ++ propFormatName)
    formatMissing <- not <$> doesFileExist formatFile

    if formatMissing
      then return $ Left $ commandIncorrectFormatSpec formatFile
      else do
        res <- do
          format <- readFile formatFile

          -- All of the following operations use Either to return error
          -- messages.  The use of the monadic bind to pass arguments from one
          -- function to the next will cause the program to stop at the
          -- earliest error.
          if | isPrefixOf "XMLFormat" format
             -> do let xmlFormat = read format
                   content <- readFile fp
                   parseXMLSpec
                     (wrapper) (def) xmlFormat content
                     -- (fmap (fmap print) . wrapper) (print def) xmlFormat content
             | otherwise
             -> do let jsonFormat = read format
                   content <- B.safeReadFile fp
                   case content of
                     Left e  -> return $ Left e
                     Right b -> do case eitherDecode b of
                                     Left e  -> return $ Left e
                                     Right v ->
                                       parseJSONSpec
                                         (wrapper)
                                         jsonFormat
                                         v
        case res of
          Left e  -> return $ Left $ cannotOpenInputFile fp
          Right x -> return $ Right x

-- | Process a variable selection file, if available, and return the variable
-- names.
parseVariablesFile :: Maybe FilePath
                   -> ExceptT ErrorTriplet IO (Maybe [String])
parseVariablesFile Nothing   = return Nothing
parseVariablesFile (Just fp) = do
  -- Fail if the file cannot be opened.
  varNamesE <- liftIO $ E.try $ lines <$> readFile fp
  case (varNamesE :: Either E.SomeException [String]) of
    Left _         -> throwError $ cannotOpenVarFile fp
    Right varNames -> return $ Just varNames

-- | Process a requirements / handlers list file, if available, and return the
-- handler names.
parseRequirementsListFile :: Maybe FilePath
                          -> ExceptT ErrorTriplet IO (Maybe [String])
parseRequirementsListFile Nothing   = return Nothing
parseRequirementsListFile (Just fp) =
  ExceptT $ makeLeftE (cannotOpenHandlersFile fp) <$>
    (E.try $ Just . lines <$> readFile fp)

-- | Process a variable database file, if available, and return the rows in it.
parseVarDBFile :: Read a
               => Maybe FilePath
               -> ExceptT ErrorTriplet IO [a]
parseVarDBFile Nothing   = return []
parseVarDBFile (Just fn) =
  ExceptT $ makeLeftE (cannotOpenDB fn) <$>
    (E.try $ fmap read <$> lines <$> readFile fn)

openVarDBFiles :: [FilePath]
               -> ExceptT ErrorTriplet IO VariableDB
openVarDBFiles files = do
  dataDir <- liftIO $ getDataDir
  let defaultDB = dataDir </> "variable-db.json"
  openVarDBFiles' emptyVariableDB (files ++ [defaultDB])

openVarDBFiles' :: VariableDB
                -> [FilePath]
                -> ExceptT ErrorTriplet IO VariableDB
openVarDBFiles' acc []     = return acc
openVarDBFiles' acc (x:xs) = do
  file <- parseVarDBFile' (Just x)
  acc' <- mergeVariableDB acc file
  openVarDBFiles' acc' xs

parseVarDBFile' :: Maybe FilePath
                -> ExceptT ErrorTriplet IO VariableDB
parseVarDBFile' Nothing   = return emptyVariableDB
parseVarDBFile' (Just fn) =
  ExceptT $ makeLeftE' (cannotOpenDB fn) <$>
    (eitherDecodeFileStrict fn)

-- | Process a JSON file with additional template variables to make available
-- during template expansion.
parseTemplateVarsFile :: Maybe FilePath
                      -> ExceptT ErrorTriplet IO Value
parseTemplateVarsFile Nothing   = return $ object []
parseTemplateVarsFile (Just fp) = do
  content <- liftIO $ B.safeReadFile fp
  let value = eitherDecode =<< content
  case value of
    Right x@(Object _) -> return x
    Right x@Null       -> return x
    Right _            -> throwError (cannotReadObjectTemplateVars fp)
    _                  -> throwError (cannotOpenTemplateVars fp)

-- | Check that the arguments provided are sufficient to operate.
--
-- The backend provides several modes of operation, which are selected
-- by providing different arguments to the `ros` command.
--
-- When an input specification file is provided, the variables and requirements
-- defined in it are used unless variables or handlers files are provided, in
-- which case the latter take priority.
--
-- If an input file is not provided, then the user must provide BOTH a variable
-- list, and a list of handlers.
checkArguments :: Maybe (Spec a)
               -> Maybe [String]
               -> Maybe [String]
               -> Either ErrorTriplet ()
checkArguments Nothing Nothing   Nothing   = Left wrongArguments
checkArguments Nothing Nothing   _         = Left wrongArguments
checkArguments Nothing _         Nothing   = Left wrongArguments
checkArguments _       (Just []) _         = Left wrongArguments
checkArguments _       _         (Just []) = Left wrongArguments
checkArguments _       _         _         = Right ()

-- | Extract the variables from a specification, and sanitize them.
specExtractExternalVariables :: Maybe (Spec a) -> [String]
specExtractExternalVariables Nothing   = []
specExtractExternalVariables (Just cs) = map sanitizeLCIdentifier
                                       $ map externalVariableName
                                       $ externalVariables cs

-- | Extract the requirements from a specification, and sanitize them to match
-- the names of the handlers used by Copilot.
specExtractHandlers :: Maybe (Spec a) -> [String]
specExtractHandlers Nothing   = []
specExtractHandlers (Just cs) = map handlerNameF
                              $ map requirementName
                              $ requirements cs
  where
    handlerNameF = ("handler" ++) . sanitizeUCIdentifier

-- * Handler for boolean expressions

-- | Handler for boolean expressions that knows how to parse them, replace
-- variables in them, and convert them to Copilot.
--
-- It also contains a default value to be used whenever an expression cannot be
-- found in the input file.
data ExprPair = forall a . ExprPair
  { exprTPair   :: ExprPairT a
  }

data ExprPairT a = ExprPairT
  { exprTParse   :: String -> Either String a
  , exprTReplace :: [(String, String)] -> a -> a
  , exprTPrint   :: a -> String
  , exprTIdents  :: a -> [String]
  , exprTUnknown :: a
  }


-- | Return a handler depending on whether it should be for CoCoSpec boolean
-- expressions or for SMV boolean expressions. We default to SMV if not format
-- is given.
exprPair :: String -> ExprPair
exprPair "cocospec" = ExprPair $
  ExprPairT
    (CoCoSpec.pBoolSpec . CoCoSpec.myLexer)
    (\_ -> id)
    (CoCoSpec.boolSpec2Copilot)
    (CoCoSpec.boolSpecNames)
    (CoCoSpec.BoolSpecSignal (CoCoSpec.Ident "undefined"))
exprPair "literal" = ExprPair $
  ExprPairT
    Right
    (\_ -> id)
    id
    (const [])
    "undefined"
exprPair _ = ExprPair $
  ExprPairT
    (SMV.pBoolSpec . SMV.myLexer)
    (substituteBoolExpr)
    (SMV.boolSpec2Copilot)
    (SMV.boolSpecNames)
    (SMV.BoolSpecSignal (SMV.Ident "undefined"))

-- * Errors

-- | A triplet containing error information.
data ErrorTriplet = ErrorTriplet ErrorCode String Location

-- | Encoding of reasons why the command can fail.
--
-- The error codes used are 1 for user error, and 2 for internal bug.
type ErrorCode = Int

-- | Process a computation that can fail with an error code, and turn it into a
-- computation that returns a 'Result'.
processResult :: Monad m => ExceptT ErrorTriplet m a -> m (Result ErrorCode)
processResult m = do
  r <- runExceptT m
  case r of
    Left (ErrorTriplet errorCode msg location)
      -> return $ Error errorCode msg location
    _ -> return Success

-- ** Error messages

-- | Exception handler to deal with the case in which the arguments
-- provided are incorrect.
wrongArguments :: ErrorTriplet
wrongArguments =
    ErrorTriplet ecWrongArguments msg LocationNothing
  where
    msg =
      "the arguments provided are insufficient: you must provide an input "
      ++ "specification, or both a variables and a handlers file."

-- | Exception handler to deal with the case in which the input file cannot be
-- opened.
cannotOpenInputFile :: FilePath -> ErrorTriplet
cannotOpenInputFile file =
    ErrorTriplet ecCannotOpenInputFile msg (LocationFile file)
  where
    msg =
      "cannot open input specification file " ++ file

-- | Exception handler to deal with the case in which the variable DB cannot be
-- opened.
cannotOpenDB :: FilePath -> ErrorTriplet
cannotOpenDB file =
    ErrorTriplet ecCannotOpenDBFile msg (LocationFile file)
  where
    msg =
      "cannot open variable DB file " ++ file

-- | Exception handler to deal with the case in which the variable file
-- provided by the user cannot be opened.
cannotOpenVarFile :: FilePath -> ErrorTriplet
cannotOpenVarFile file =
    ErrorTriplet ecCannotOpenVarFile  msg (LocationFile file)
  where
    msg =
      "cannot open variable list file " ++ file

-- | Exception handler to deal with the case in which the handlers file cannot
-- be opened.
cannotOpenHandlersFile :: FilePath -> ErrorTriplet
cannotOpenHandlersFile file =
    ErrorTriplet ecCannotOpenHandlersFile msg (LocationFile file)
  where
    msg =
      "cannot open handlers file " ++ file

-- | Error message associated to the format file not being found.
commandIncorrectFormatSpec :: FilePath -> ErrorTriplet
commandIncorrectFormatSpec formatFile =
    ErrorTriplet ecIncorrectFormatFile msg (LocationFile formatFile)
  where
    msg =
      "The format specification " ++ formatFile ++ " does not exist or is not "
      ++ "readable"

-- | Exception handler to deal with the case in which the template vars file
-- cannot be opened.
cannotOpenTemplateVars :: FilePath -> ErrorTriplet
cannotOpenTemplateVars file =
    ErrorTriplet ecCannotOpenTemplateVarsFile msg (LocationFile file)
  where
    msg =
      "Cannot open file with additional template variables: " ++ file

-- | Exception handler to deal with the case in which the template vars file
-- cannot be opened.
cannotReadObjectTemplateVars :: FilePath -> ErrorTriplet
cannotReadObjectTemplateVars file =
    ErrorTriplet ecCannotReadObjectTemplateVarsFile msg (LocationFile file)
  where
    msg =
      "Cannot open file with additional template variables: " ++ file

-- | Exception handler to deal with the case of files that cannot be
-- copied/generated due lack of space or permissions or some I/O error.
cannotCopyTemplate :: ErrorTriplet
cannotCopyTemplate =
    ErrorTriplet ecCannotCopyTemplate msg LocationNothing
  where
    msg =
      "Generation failed during copy/write operation. Check that"
      ++ " there's free space in the disk and that you have the necessary"
      ++ " permissions to write in the destination directory."

-- ** Error codes

-- | Error: wrong arguments provided.
ecWrongArguments :: ErrorCode
ecWrongArguments = 1

-- | Error: the input specification provided by the user cannot be opened.
ecCannotOpenInputFile :: ErrorCode
ecCannotOpenInputFile = 1

-- | Error: the variable DB provided by the user cannot be opened.
ecCannotOpenDBFile :: ErrorCode
ecCannotOpenDBFile = 1

-- | Error: the variable file provided by the user cannot be opened.
ecCannotOpenVarFile :: ErrorCode
ecCannotOpenVarFile = 1

-- | Error: the handlers file provided by the user cannot be opened.
ecCannotOpenHandlersFile :: ErrorCode
ecCannotOpenHandlersFile = 1

-- | Error: the format file cannot be opened.
ecIncorrectFormatFile :: ErrorCode
ecIncorrectFormatFile = 1

-- | Error: the template vars file provided by the user cannot be opened.
ecCannotOpenTemplateVarsFile :: ErrorCode
ecCannotOpenTemplateVarsFile = 1

-- | Error: the template variables file passed does not contain a JSON object.
ecCannotReadObjectTemplateVarsFile :: ErrorCode
ecCannotReadObjectTemplateVarsFile = 1

-- | Error: the files cannot be copied/generated due lack of space or
-- permissions or some I/O error.
ecCannotCopyTemplate :: ErrorCode
ecCannotCopyTemplate = 1

-- * Auxiliary Functions

-- | Return the path to the template directory.
locateTemplateDir :: Maybe FilePath
                  -> FilePath
                  -> ExceptT e IO FilePath
locateTemplateDir mTemplateDir name =
  case mTemplateDir of
    Just x  -> return x
    Nothing -> liftIO $ do
      dataDir <- getDataDir
      return $ dataDir </> "templates" </> name

-- | Parse a property using an auxiliary program to first translate it, if
-- available.
--
-- If a program is given, it is first called on the property, and then the
-- result is parsed with the parser passed as an argument. If a program is not
-- given, then the parser is applied to the given string.
wrapVia :: Maybe String                -- ^ Auxiliary program to translate the
                                       -- property.
        -> (String -> Either String a) -- ^ Parser used on the result.
        -> String                      -- ^ Property to parse.
        -> IO (Either String a)
wrapVia Nothing  parse s = return (parse s)
wrapVia (Just f) parse s =
  E.handle (\(e :: E.IOException) -> return $ Left $ show e) $ do
    out <- readProcess f [] s
    return $ parse out

-- | Merge two JSON objects.
--
-- Fails if the values are not objects or null.
mergeObjects :: Value -> Value -> Value
mergeObjects (Object m1) (Object m2) = Object (union m1 m2)
mergeObjects obj         Null        = obj
mergeObjects Null        obj         = obj
mergeObjects _           _           = error "The values passed are not objects"

-- | Replace the left Exception in an Either.
makeLeftE :: c -> Either E.SomeException b -> Either c b
makeLeftE c (Left _)   = Left c
makeLeftE _ (Right x)  = Right x

-- | Replace the left Exception in an Either.
makeLeftE' :: c -> Either a b -> Either c b
makeLeftE' c (Left _)   = Left c
makeLeftE' _ (Right x)  = Right x

-- Parse Variable DBs

findInput :: VariableDB -> String -> Maybe InputDef
findInput varDB inputName =
  find (\x -> name x == inputName) (inputs varDB)

findConnection :: InputDef -> String -> Maybe Connection
findConnection inputDef connectionName =
  find (\x -> scope x == connectionName) (connections inputDef)

findTopic :: VariableDB -> String -> String -> Maybe TopicDef
findTopic varDB scope name =
  find (\x -> topicScope x == scope && topicTopic x == name) (topics varDB)

findType :: VariableDB -> String -> String -> String -> Maybe TypeDef
findType varDB inputName connectionName destConn = do
  inputDef      <- findInput varDB inputName
  connectionDef <- findConnection inputDef connectionName
  topic         <- findTopic varDB connectionName (topic connectionDef)

  let match :: TypeDef -> Bool
      match typeDef = fromScope typeDef == connectionName
                   && fromType typeDef == topicType topic
                   && fromField typeDef == field connectionDef
                   && toScope typeDef == destConn

  find match (types varDB)

mergeVariableDB :: Monad m
                => VariableDB -> VariableDB -> ExceptT ErrorTriplet m VariableDB
mergeVariableDB varDB1 varDB2 = do
  inputs' <- mergeInputs (inputs varDB1)    (inputs varDB2)
  topics' <- mergeTopics (topics varDB1)    (topics varDB2)
  types'  <- mergeTypes  (types varDB1)     (types varDB2)
  outputs' <- mergeOutputs (outputs varDB1) (outputs varDB2)
  return $ VariableDB inputs' topics' types' outputs'

mergeInputs :: Monad m
            => [InputDef] -> [InputDef] -> ExceptT ErrorTriplet m [InputDef]
mergeInputs inputs []     = return inputs
mergeInputs inputs (x:xs) = do
  inputs' <- mergeInput inputs x
  mergeInputs inputs' xs

mergeInput :: Monad m
           => [InputDef] -> InputDef -> ExceptT ErrorTriplet m [InputDef]
mergeInput []     i = return [i]
mergeInput (x:xs) i
  | name x == name i
  = do cs <- mergeConnections (connections x) (connections i)
       return $ InputDef (name x) cs : xs

  | otherwise
  = do xs' <- mergeInput xs i
       return $ x : xs'

mergeConnections :: Monad m
                 => [Connection] -> [Connection] -> ExceptT ErrorTriplet m [Connection]
mergeConnections cs []     = return cs
mergeConnections cs (x:xs) = do
  cs' <- mergeConnection cs x
  mergeConnections cs' xs

mergeConnection ::  Monad m
                => [Connection] -> Connection -> ExceptT ErrorTriplet m [Connection]
mergeConnection []     c = return [c]
mergeConnection (x:xs) c
  | x == c
  = return $ x : xs

  | scope x == scope c
  = throwError $
      ErrorTriplet 1 "Cannot merge different connections with the same scope" LocationNothing

  | otherwise
  = do cs' <- mergeConnection xs c
       return (x : cs')

mergeTopics :: Monad m
            => [TopicDef] -> [TopicDef] -> ExceptT ErrorTriplet m [TopicDef]
mergeTopics ts [] = return ts
mergeTopics ts (x:xs) = do
  ts' <- mergeTopic ts x
  mergeTopics ts' xs

mergeTopic :: Monad m
           => [TopicDef] -> TopicDef -> ExceptT ErrorTriplet m [TopicDef]
mergeTopic []     c = return [c]
mergeTopic (x:xs) c
  | x == c
  = return $ x : xs

  | topicScope x == topicScope c && topicTopic x == topicTopic c
  = throwError $
      ErrorTriplet 1 "Cannot merge topics with same scope and different types" LocationNothing

  | otherwise
  = do ts' <- mergeTopic xs c
       return (x : ts')

mergeTypes :: Monad m
           => [TypeDef] -> [TypeDef] -> ExceptT ErrorTriplet m [TypeDef]
mergeTypes ts [] = return ts
mergeTypes ts (x:xs) = do
  ts' <- mergeType ts x
  mergeTypes ts' xs

mergeType :: Monad m
           => [TypeDef] -> TypeDef -> ExceptT ErrorTriplet m [TypeDef]
mergeType []     c = return [c]
mergeType (x:xs) c
  | x == c
  = return $ x : xs

  | fromScope x == fromScope c && fromType x == fromType c &&
    toScope x == toScope c
  = throwError $
      ErrorTriplet 1 "Cannot merge types with same scopes and from type" LocationNothing

  | otherwise
  = do ts' <- mergeType xs c
       return (x : ts')

mergeOutputs :: Monad m
             => [OutputDef] -> [OutputDef] -> ExceptT ErrorTriplet m [OutputDef]
mergeOutputs ts [] = return ts
mergeOutputs ts (x:xs) = do
  ts' <- mergeOutput ts x
  mergeOutputs ts' xs

mergeOutput :: Monad m
            => [OutputDef] -> OutputDef -> ExceptT ErrorTriplet m [OutputDef]
mergeOutput []     o = return [o]
mergeOutput (x:xs) o
  | x == o
  = return $ x : xs

  | outputName x == outputName o
  = throwError $
      ErrorTriplet 1 "Cannot merge outputs with the same name and different args" LocationNothing

  | otherwise
  = do xs' <- mergeOutput xs o
       return (x : xs')

data VariableDB = VariableDB
    { inputs  :: [InputDef]
    , topics  :: [TopicDef]
    , types   :: [TypeDef]
    , outputs :: [OutputDef]
    }
  deriving (Generic, Show)

instance FromJSON VariableDB

emptyVariableDB :: VariableDB
emptyVariableDB = VariableDB [] [] [] []

data InputDef = InputDef
    { name        :: String
    , connections :: [ Connection ]
    }
  deriving (Eq, Generic, Show)

instance FromJSON InputDef

data Connection = Connection
    { scope :: String
    , topic :: String
    , field :: Maybe String
    }
  deriving (Eq, Generic, Show)

instance FromJSON Connection

data TopicDef = TopicDef
    { topicScope :: String
    , topicTopic :: String
    , topicType  :: String
    }
  deriving (Eq, Show)

instance FromJSON TopicDef where
  parseJSON (Object v) = TopicDef
    <$> v .: "scope"
    <*> v .: "topic"
    <*> v .: "type"
  parseJSON invalid =
    prependFailure "parsing topic definition failed: "
      (typeMismatch "Object" invalid)

data TypeDef = TypeDef
    { fromScope :: String
    , fromType  :: String
    , fromField :: Maybe String
    , toScope   :: String
    , toType    :: String
    }
  deriving (Eq, Generic, Show)

instance FromJSON TypeDef

data OutputDef = OutputDef
    { outputName :: String
    , outputType :: Maybe String
    }
  deriving (Eq, Show)

instance FromJSON OutputDef where
  parseJSON (Object v) = OutputDef
    <$> v .: "name"
    <*> v .: "type"

  parseJSON invalid =
    prependFailure "parsing output definition failed: "
      (typeMismatch "Object" invalid)

--  { "inputs":
--     [ { "name": "variable1"
--       , "type": "double"
--       , "connections":
--          [ { "scope": "cfs"
--            , "topic": "MY_APP_MSG_ID"
--            }
--          , { "scope": "ros/message"
--            , "topic": "/some_app/topic"
--            }
--          ]
--       }
--     ]
-- , "topics":
--     [ { "scope": "cfs"
--       , "topic": "MY_APP_MSG_ID"
--       , "type":  "my_app_msg_t"
--       }
--     , { "scope": "ros/message"
--       , "topic": "/some_app/topic"
--       , "type":  "std_msgs::msg::Float64"
--       }
--     ]
-- , "type_mappings":
--     [ { "scope1": "cfs"
--       , "type1":  "my_app_msg_t"
--       , "field1": "data_field"
--       , "scope2": "C"
--       , "type2":  "double"
--       }
--     , { "scope1": "ros/message"
--       , "type1":  "std_msgs::msg::Float64"
--       , "field1": "data"
--       , "scope2": "C"
--       , "type2":  "double"
--       }
--     , { "scope1": "ros/variable"
--       , "type1":  "std::int64_t"
--       , "field1": "data"
--       , "scope2": "C"
--       , "type2":  "int64_t"
--       }
--     ]
-- , "outputs":
--     [ { "name": "output1"
--       , "type": "double"
--       }
--     , { "name": "output2"
--       }
--     ]
-- }
