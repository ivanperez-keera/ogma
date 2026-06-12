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
-- | Formal analysis of diagrams.
module Data.Diagram.Analysis
    ( analyzeDiagram
    , AnalysisResult(..)
    )
  where

-- External imports
import qualified Copilot.Core as Core

-- Internal imports: auxiliary
import Copilot.Core.Analysis          (exprIsConstant)
import Copilot.Language.Reify.Extra   (reifySpec)
import Data.Diagram                   (Diagram (..),
                                       diagramNumStates)
import Language.Trans.Diagram2Copilot (diagram2Copilot)

-- * Analysis of Specs

-- | Result of analyzing a diagram.
data AnalysisResult = AnalysisResult
  { numStates     :: Int  -- ^ Number of states in the diagram.
  , deterministic :: Bool -- ^ True if the diagram is deterministic.
  }

-- | Formally analyze a diagram.
analyzeDiagram :: Diagram -> IO AnalysisResult
analyzeDiagram diagram = do

  let nStates = diagramNumStates diagram

  coreSpec <- reifySpec defaultSpecImports $ showDiagram diagram

  let properties     = zip ["deterministic"] propertyGuards
      propertyGuards = map Core.triggerGuard $ Core.specTriggers coreSpec

  constantProperties <- mapM (uncurry $ exprIsConstant coreSpec) properties

  let numTrue = length $ filter fst constantProperties

  let diagramDeterministic = numTrue > 0

  return $ AnalysisResult nStates diagramDeterministic

-- | Default imports for a 'Spec' that was converted into a 'Copilot.Spec'.
defaultSpecImports :: [(String, Maybe String)]
defaultSpecImports =
  [ ("Control.Monad.Writer",          Nothing)
  , ("Copilot.Language",              Nothing)
  , ("Copilot.Language.Spec",         Nothing)
  , ("Copilot.Library.StateMachines", Nothing)
  , ("Data.Functor.Identity",         Nothing)
  , ("Data.List",                     Just "L")
  , ("Prelude",                       Just "P")
  ]

-- | Render a 'Diagram' as a Haskell definition of a 'Copilot.Spec'.
--
-- The shown 'Copilot.Spec' has a top-level triggers for the properties we are
-- interested in, as well as several auxiliary definitions.
showDiagram :: Diagram -> String
showDiagram diagram = unlines $
       [ "do let" ]
    ++ map ("       " ++) (lines auxiliaryDefinitions)
    ++ map ("       " ++) (lines input)
    ++ map ("       " ++) (lines diagramDefinitions)
    ++ [ "   trigger \"deterministic\" (isDeterministic stateMachine1) []" ]

  where

    input = unlines
      [ "input :: Stream Word8"
      , "input = extern \"input\" Nothing"
      ]

    diagramDefinitions = diagram2Copilot diagram

-- | Auxiliary definitions needed in the spec.
auxiliaryDefinitions :: String
auxiliaryDefinitions = unlines
  [ "isDeterministic :: (Ord a, Eq a, Typed a)"
  , "                => (a, a, Stream Bool, [(a, Stream Bool, a)], a)"
  , "                -> Stream Bool"
  , "isDeterministic (_, _, _, ts, _) = all $"
  , "    map"
  , "      (\\s -> all (map not $ pairwise $ transitionsFrom s))"
  , "      states"
  , "  where"
  , "    states = L.nub $ L.sort $ concat $"
  , "      map (\\(s1, _, s2) -> [s1, s2]) ts"
  , ""
  , "    transitionsFrom s = [ t | (s1, t, _) <- ts, s P.== s1 ]"
  , ""
  , "completeMachine :: (Ord a, Eq a, Typed a)"
  , "                => (a, a, Stream Bool, [(a, Stream Bool, a)], a)"
  , "                -> Stream Bool"
  , "completeMachine (_, _, _, ts, _) = all $"
  , "    map (\\s -> or $ transitionsFrom s) states"
  , "  where"
  , "    states = L.nub $ L.sort $ concat $"
  , "      map (\\(s1, _, s2) -> [s1, s2]) ts"
  , "    transitionsFrom s = [ t | (s1, t, _) <- ts, s P.== s1 ]"
  , ""
  , "all [] = true"
  , "all (x:xs) = x && all xs"
  , ""
  , "or [] = false"
  , "or (x:xs) = x || or xs"
  , ""
  , "pairwise :: [ Stream Bool ] -> [ Stream Bool ]"
  , "pairwise []      = []"
  , "pairwise (x:[])  = []"
  , "pairwise (x1:xs) ="
  , "  (map (\\x2 -> (x1 && x2)) xs) P.++ pairwise xs"
  ]
