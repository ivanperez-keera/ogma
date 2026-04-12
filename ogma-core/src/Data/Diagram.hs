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
-- | Diagrams.
module Data.Diagram
    ( Diagram(..)
    , diagramStates
    , diagramNumStates
    )
  where

-- External imports
import Data.List (nub, sort)

-- | Internal representation for diagrams.
newtype Diagram = Diagram
    { diagramTransitions :: [(Int, String, Int)]
    }
  deriving (Show, Eq)

-- | States in a diagram.
diagramStates :: Diagram -> [Int]
diagramStates diagram = nub $ sort $ concat
  [ [s, d] | (s, _, d) <- diagramTransitions diagram ]

-- | Number of states in a diagram.
diagramNumStates :: Diagram -> Int
diagramNumStates = length . diagramStates
