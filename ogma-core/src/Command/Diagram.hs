{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
-- Copyright 2024 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at
--
--      https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
--
-- | Transform a state diagram into a Copilot specification.
module Command.Diagram
    ( diagram
    , DiagramOptions(..)
    , DiagramFormat(..)
    , DiagramMode(..)
    , DiagramPropFormat(..)
    , ErrorCode
    )
  where

-- External imports
import Control.Exception as E
import Data.Aeson        (object, (.=))
import Data.Foldable     (for_)
import Data.Functor      ((<&>))
import Data.List         (intercalate, nub, sort)
import Data.Text.Lazy    (pack)
import System.FilePath   ((</>))

-- External imports: auxiliary
import System.Directory.Extra ( copyTemplate )

-- External imports: parsing expressions.
import qualified Language.Lustre.AbsLustre as Lustre
import qualified Language.Lustre.ParLustre as Lustre (myLexer, pBoolSpec)
import qualified Language.SMV.AbsSMV       as SMV
import qualified Language.SMV.ParSMV       as SMV (myLexer, pBoolSpec)

-- Internal imports: auxiliary
import Command.Common        (ExprPair (..), ExprPairT (..))
import Command.CommonDiagram (Diagram (..), DiagramFormat (..), readDiagram)
import Command.Result        (Result (..))
import Data.Location         (Location (..))
import Paths_ogma_core       (getDataDir)

-- Internal imports: language ASTs, transformers
import           Language.SMV.Substitution     (substituteBoolExpr)
import qualified Language.Trans.Lustre2Copilot as Lustre (boolSpec2Copilot,
                                                          boolSpecNames)
import           Language.Trans.SMV2Copilot    as SMV (boolSpec2Copilot,
                                                       boolSpecNames)

-- | Generate a new Copilot monitor that implements a state machine described
-- in a diagram given as an input file.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @stateMachine@, @externalState@, @main@, @spec@,
-- @stateMachine1@, @clock@, @ftp@, @notPreviousNot@. All identifiers used are
-- valid C99 identifiers. The template, if provided, exists and uses the
-- variables needed by the diagram application generator. The target directory
-- is writable and there's enough disk space to copy the files over.
diagram :: FilePath       -- ^ Path to a file containing a diagram
        -> DiagramOptions -- ^ Customization options
        -> IO (Result ErrorCode)
diagram fp options = do
  E.handle (return . diagramTemplateError fp) $ do
    -- Sub-parser for edge expressions.
    let functions = exprPair (diagramPropFormat options)

    -- Convert the diagram into elements in a Copilot spec.
    copilotSpecElems <- diagram' fp options functions

    -- Convert the elements into a success or error result.
    let (mOutput, result) = diagramResult fp copilotSpecElems

    -- If the result is success, expand the template.
    for_ mOutput $ \(streamDefs, handlerInputs) -> do
      let subst = object
                    [ "streamDefs"    .= pack streamDefs
                    , "specName"      .= pack (diagramFilename options)
                    , "input"         .= pack (diagramInputVar options)
                    , "state"         .= pack (diagramStateVar options)
                    , "handlerInputs" .= pack handlerInputs
                    ]

      templateDir <- case diagramTemplateDir options of
                       Just x  -> return x
                       Nothing -> do
                         dataDir <- getDataDir
                         return $ dataDir </> "templates" </> "diagram"

      let targetDir = diagramTargetDir options

      copyTemplate templateDir subst targetDir

    return result

-- | Generate a new Copilot monitor that implements a state machine described
-- in a diagram given as an input file, using a subexpression handler.
--
-- PRE: The file given is readable, contains a valid file with recognizable
-- format, the formulas in the file do not use any identifiers that exist in
-- Copilot, or any of @stateMachine@, @externalState@, @main@, @spec@,
-- @stateMachine1@, @clock@, @ftp@, @notPreviousNot@. All identifiers used are
-- valid C99 identifiers. The template, if provided, exists and uses the
-- variables needed by the diagram application generator. The target directory
-- is writable and there's enough disk space to copy the files over.
diagram' :: FilePath
         -> DiagramOptions
         -> ExprPair
         -> IO (Either String (String, String))
diagram' fp options exprP = do
  diagramE <- readDiagram fp (diagramFormat options) exprP
  pure $ diagramE <&> \diagramR ->
    diagramToCopilot diagramR (diagramMode options)

-- | Options used to customize the conversion of diagrams to Copilot code.
data DiagramOptions = DiagramOptions
  { diagramTargetDir   :: FilePath
  , diagramTemplateDir :: Maybe FilePath
  , diagramFormat      :: DiagramFormat
  , diagramPropFormat  :: DiagramPropFormat
  , diagramFilename    :: String
  , diagramMode        :: DiagramMode
  , diagramStateVar    :: String
  , diagramInputVar    :: String
  }

-- | Modes of operation.
data DiagramMode = CheckState   -- ^ Check if given state matches expectation
                 | ComputeState -- ^ Compute expected state
                 | CheckMoves   -- ^ Check if transitioning to a state would be
                                --   possible.
  deriving (Eq, Show)

-- | Property formats supported.
data DiagramPropFormat = Lustre
                       | Inputs
                       | Literal
                       | SMV
  deriving (Eq, Show)

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error.
type ErrorCode = Int

-- | Error: the input file cannot be read due to it being unreadable or the
-- format being incorrect.
ecDiagramError :: ErrorCode
ecDiagramError = 1

-- | Error: diagram component generation failed during the copy/write
-- process.
ecDiagramTemplateError :: ErrorCode
ecDiagramTemplateError = 2

-- * Result

-- | Process the result of the transformation function.
diagramResult :: FilePath
              -> Either String a
              -> (Maybe a, Result ErrorCode)
diagramResult fp result = case result of
  Left msg -> (Nothing, Error ecDiagramError msg (LocationFile fp))
  Right t  -> (Just t,  Success)

-- | Report an error when trying to open or copy the template.
diagramTemplateError :: FilePath
                     -> E.SomeException
                     -> Result ErrorCode
diagramTemplateError fp exception =
    Error ecDiagramTemplateError msg (LocationFile fp)
  where
    msg =
      "Diagram monitor generation failed during copy/write operation. Check"
      ++ " that there's free space in the disk and that you have the necessary"
      ++ " permissions to write in the destination directory. "
      ++ show exception

-- * Handler for boolean expressions in edges or transitions between states.

-- | Return a handler depending on the format used for edge or transition
-- properties.
exprPair :: DiagramPropFormat -> ExprPair
exprPair Lustre = ExprPair $
  ExprPairT
    (Lustre.pBoolSpec . Lustre.myLexer)
    (\_ -> id)
    Lustre.boolSpec2Copilot
    Lustre.boolSpecNames
    (Lustre.BoolSpecSignal (Lustre.Ident "undefined"))
exprPair Inputs = ExprPair $
  ExprPairT
    ((Right . read) :: String -> Either String Int)
    (\_ -> id)
    (\x -> "input == " ++ show x)
    (const [])
    (-1)
exprPair Literal = ExprPair $
  ExprPairT
    Right
    (\_ -> id)
    id
    (const [])
    "undefined"
exprPair SMV = ExprPair $
  ExprPairT
    (SMV.pBoolSpec . SMV.myLexer)
    substituteBoolExpr
    SMV.boolSpec2Copilot
    SMV.boolSpecNames
    (SMV.BoolSpecSignal (SMV.Ident "undefined"))

-- * Backend

-- | Convert the diagram into a set of Copilot definitions, and a list of
-- arguments for the top-level handler.
diagramToCopilot :: Diagram -> DiagramMode -> (String, String)
diagramToCopilot diag mode = (machine, arguments)
  where
    machine = unlines
      [ "stateMachineProp :: Stream Bool"
      , "stateMachineProp = " ++ propExpr
      , ""
      , "stateMachine1 :: Stream Word8"
      , "stateMachine1 = stateMachineGF (initialState, finalState, noInput, "
        ++ "transitions, badState)"
      , ""
      , "-- Check"
      , "initialState :: Word8"
      , "initialState = " ++ show initialState
      , ""
      , "-- Check"
      , "finalState :: Word8"
      , "finalState = " ++ show finalState
      , ""
      , "noInput :: Stream Bool"
      , "noInput = false"
      , ""
      , "badState :: Word8"
      , "badState = " ++ show badState
      , ""
      , "transitions = " ++ showTransitions
      ]

    -- Elements of the spec.
    propExpr     = case mode of
                     CheckState   -> "stateMachine1 /= externalState"
                     ComputeState -> "true"
                     CheckMoves   -> "true"
    initialState = minimum states
    finalState   = maximum states
    badState     = maximum states + 1

    -- Arguments for the handler.
    arguments = "[ " ++ intercalate ", " (map ("arg " ++) argExprs) ++ " ]"

    argExprs = case mode of
      CheckState   -> [ "stateMachine1", "externalState", "input" ]
      ComputeState -> [ "stateMachine1", "externalState", "input" ]
      CheckMoves   -> map stateCheckExpr states

    stateCheckExpr stateId =
      "(checkValidTransition transitions externalState " ++ show stateId ++ ")"

    -- States and transitions from the diagram.
    transitions = diagramTransitions diag
    states      = nub $ sort $ concat [ [x, y] | (x, _, y) <- transitions ]

    showTransitions :: String
    showTransitions = "[" ++ showTransitions' transitions

    showTransitions' :: [(Int, String, Int)] -> String
    showTransitions' []         = "]"
    showTransitions' (x1:x2:xs) =
      showTransition x1 ++ ", " ++ showTransitions' (x2:xs)
    showTransitions' (x2:[])    = showTransition x2 ++ "]"

    showTransition :: (Int, String, Int) -> String
    showTransition (a, b, c) =
      "(" ++ show a ++ ", " ++ b ++ ", " ++ show c ++ ")"
