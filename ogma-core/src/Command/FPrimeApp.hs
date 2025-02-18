{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric             #-}
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
-- | Create <https://github.com/nasa/fprime FPrime> components that subscribe
-- to obtain data and call Copilot when new values arrive.

{- HLINT ignore "Functor law" -}
module Command.FPrimeApp
    ( command
    , CommandOptions(..)
    , ErrorCode
    )
  where

-- External imports
import           Control.Applicative    ( liftA2 )
import qualified Control.Exception      as E
import           Control.Monad.Except   ( ExceptT(..), liftEither )
import           Data.Aeson             ( ToJSON, toJSON )
import           Data.Char              ( toUpper )
import           Data.List              ( find )
import           Data.Maybe             ( fromMaybe, mapMaybe )
import           GHC.Generics           ( Generic )

-- External imports: auxiliary
import System.Directory.Extra ( copyTemplate )

import qualified Command.Standalone

-- Internal imports: auxiliary
import Command.Result (Result (..))

-- Internal imports
import Command.Common


-- | Generate a new FPrime component connected to Copilot.
command :: CommandOptions -- ^ Options to the ROS backend.
        -> IO (Result ErrorCode)
command options = processResult $ do
    -- Obtain template dir
    templateDir <- locateTemplateDir mTemplateDir "fprime"

    templateVars <- parseTemplateVarsFile templateVarsF

    appData <- command' options functions

    let subst = mergeObjects (toJSON appData) templateVars

    -- Expand template
    ExceptT $ fmap (makeLeftE cannotCopyTemplate) $ E.try $
      copyTemplate templateDir subst targetDir

  where

    targetDir     = commandTargetDir options
    mTemplateDir  = commandTemplateDir options
    functions     = exprPair (commandPropFormat options)
    templateVarsF = commandExtraVars options

command' :: CommandOptions
         -> ExprPair
         -> ExceptT ErrorTriplet IO AppData
command' options (ExprPair exprT) = do
    -- Open files needed to fill in details in the template.
    vs    <- parseVariablesFile varNameFile
    rs    <- parseRequirementsListFile handlersFile
    varDB <- parseVarDBFile varDBFile

    spec  <- maybe (return Nothing) (\f -> Just <$> parseInputFile' f) fp

    liftEither $ checkArguments spec vs rs

    copilotM <- sequenceA $ liftA2 processSpec spec fp

    let varNames = fromMaybe (specExtractExternalVariables spec) vs
        monitors = fromMaybe (specExtractHandlers spec) rs

    let appData   = AppData variables monitors' copilotM
        variables = mapMaybe (variableMap varDB) varNames
        monitors' = map (\x -> Monitor x (map toUpper x)) monitors

    return appData

  where

    fp             = commandInputFile options
    varNameFile    = commandVariables options
    varDBFile      = commandVariableDB options
    handlersFile   = commandHandlers options
    formatName     = commandFormat options
    propFormatName = commandPropFormat options
    propVia        = commandPropVia options

    parseInputFile' f =
      parseInputFile f formatName propFormatName propVia exprT

    processSpec spec' fp' =
      Command.Standalone.commandLogic fp' "copilot" [] exprT spec'

-- ** Argument processing

-- | Options used to customize the conversion of specifications to F'
-- applications.
data CommandOptions = CommandOptions
  { commandInputFile   :: Maybe FilePath -- ^ Input specification file.
  , commandTargetDir   :: FilePath       -- ^ Target directory where the
                                         -- component should be created.
  , commandTemplateDir :: Maybe FilePath -- ^ Directory where the template is
                                         -- to be found.
  , commandVariables   :: Maybe FilePath -- ^ File containing a list of
                                         -- variables to make available to
                                         -- Copilot.
  , commandVariableDB  :: Maybe FilePath -- ^ File containing a list of known
                                         -- variables with their types and the
                                         -- message IDs they can be obtained
                                         -- from.
  , commandHandlers    :: Maybe FilePath -- ^ File containing a list of
                                         -- handlers used in the Copilot
                                         -- specification. The handlers are
                                         -- assumed to receive no arguments.
  , commandFormat      :: String         -- ^ Format of the input file.
  , commandPropFormat  :: String         -- ^ Format used for input properties.
  , commandPropVia     :: Maybe String   -- ^ Use external command to
                                         -- pre-process system properties.
  , commandExtraVars   :: Maybe FilePath -- ^ File containing additional
                                         -- variables to make available to the
                                         -- template.
  }

-- | Return the variable information needed to generate declarations
-- and subscriptions for a given variable name and variable database.
variableMap :: [(String, String)]
            -> String
            -> Maybe VarDecl
variableMap varDB varName =
    csvToVarMap <$> find (sameName varName) varDB

  where

    -- True if the given variable and db entry have the same name
    sameName :: String
             -> (String, String)
             -> Bool
    sameName n (vn, _) = n == vn

    -- Convert a DB row into Variable info needed to generate the FPrime file
    csvToVarMap :: (String, String)
                -> (VarDecl)
    csvToVarMap (nm, ty) = (VarDecl nm ty (fprimeVarDeclType ty))

    fprimeVarDeclType ty = case ty of
      "uint8_t"  -> "U8"
      "uint16_t" -> "U16"
      "uint32_t" -> "U32"
      "uint64_t" -> "U64"
      "int8_t"   -> "I8"
      "int16_t"  -> "I16"
      "int32_t"  -> "I32"
      "int64_t"  -> "I64"
      "float"    -> "F32"
      "double"   -> "F64"
      def        -> def

-- | The declaration of a variable in C, with a given type and name.
data VarDecl = VarDecl
    { varDeclName       :: String
    , varDeclType       :: String
    , varDeclFPrimeType :: String
    }
  deriving Generic

instance ToJSON VarDecl

data Monitor = Monitor
    { monitorName :: String
    , monitorUC   :: String
    }
  deriving Generic

instance ToJSON Monitor

-- | Data that may be relevant to generate a ROS application.
data AppData = AppData
  { variables :: [VarDecl]
  , monitors  :: [Monitor]
  , copilot   :: Maybe Command.Standalone.AppData
  }
  deriving (Generic)

instance ToJSON AppData
