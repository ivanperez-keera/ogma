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
    ( diagram2Copilot
    )
  where

-- External imports
import Data.List (intercalate)

-- Internal imports: auxiliary
import Data.Diagram (Diagram (..), diagramBadState, diagramFinalState,
                     diagramInitialState)

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
