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
module Language.Trans.Diagram2Copilot
    ( DiagramMode(..)
    , diagram2CopilotSpec
    , diagram2Copilot
    )
  where

-- External imports
import Data.List (intercalate, nub, sort)

-- Internal imports: auxiliary
import Data.Diagram (Diagram (..), diagramBadState, diagramFinalState,
                     diagramInitialState)

-- | Modes of operation.
data DiagramMode = CheckState   -- ^ Check if given state matches expectation
                 | ComputeState -- ^ Compute expected state
                 | CheckMoves   -- ^ Check if transitioning to a state would be
                                --   possible.
  deriving (Eq, Show)

-- | Convert the diagram into a set of Copilot definitions, and a list of
-- arguments for the top-level handler.
diagram2CopilotSpec :: Diagram -> DiagramMode -> (String, String)
diagram2CopilotSpec diag mode = (machine, arguments)
  where
    machine = unlines
      [ "stateMachineS :: Stream Word8"
      , "stateMachineS = stateMachineGF stateMachine1"
      , ""
      , "stateMachineProp :: Stream Bool"
      , "stateMachineProp = " ++ propExpr
      , ""
      ]
      ++ diagram2Copilot diag

    -- Elements of the spec.
    propExpr = case mode of
                 CheckState   -> "stateMachineS /= externalState"
                 ComputeState -> "true"
                 CheckMoves   -> "true"

    -- Arguments for the handler.
    arguments = "[ " ++ intercalate ", " (map ("arg " ++) argExprs) ++ " ]"

    argExprs = case mode of
      CheckState   -> [ "stateMachineS" ]
      ComputeState -> [ "stateMachineS" ]
      CheckMoves   -> map stateCheckExpr states

    stateCheckExpr stateId =
      "(checkValidTransition transitions externalState " ++ show stateId ++ ")"

    -- States and transitions from the diagram.
    transitions = diagramTransitions diag
    states      = nub $ sort $ concat [ [x, y] | (x, _, y) <- transitions ]

-- | Convert the diagram into a set of Copilot definitions.
diagram2Copilot :: Diagram -> String
diagram2Copilot diag = unlines
    [ "stateMachine1 :: ( Word8"
    , "                 , Word8"
    , "                 , Stream Bool"
    , "                 , [(Word8, Stream Bool, Word8)]"
    , "                 , Word8"
    , "                 )"
    , "stateMachine1 ="
    , "  (initialState, finalState, noInput, transitions, badState)"
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

  where

    initialState = diagramInitialState diag
    finalState   = diagramFinalState diag
    badState     = diagramBadState diag

    transitions = diagramTransitions diag

    showTransitions :: String
    showTransitions =
      "[" ++ intercalate ", " (map showTransition transitions) ++ "]"

    showTransition :: (Int, String, Int) -> String
    showTransition (a, b, c) =
      "(" ++ show a ++ ", " ++ b ++ ", " ++ show c ++ ")"
