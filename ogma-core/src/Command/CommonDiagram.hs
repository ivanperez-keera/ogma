{-# LANGUAGE OverloadedStrings #-}
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
-- | Common functionality related to diagrams.
module Command.CommonDiagram
    ( Diagram(..)
    , DiagramFormat(..)
    , readDiagram
    , analyzeDiagram
    , AnalysisResult(..)
    )
  where

-- External imports
import           Control.Monad                     (when, void)
import qualified Copilot.Core                      as Core
import           Data.ByteString.Lazy              (toStrict)
import qualified Data.ByteString.Lazy              as B
import           Data.Either                       (isLeft)
import           Data.Functor.Identity             (Identity)
import           Data.GraphViz                     (graphEdges)
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as Attributes
import           Data.GraphViz.Commands.IO         (toUTF8)
import qualified Data.GraphViz.Parsing             as G
import           Data.GraphViz.PreProcessing       (preProcess)
import qualified Data.GraphViz.Types.Generalised   as Gs
import           Data.List                         (intercalate, nub, sort)
import qualified Data.Set                          as Set
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Data.Text.Lazy                    as LT
import           Data.Void                         (Void)
import           Text.Megaparsec                   (ErrorFancy (ErrorFail),
                                                    ParsecT, choice, empty,
                                                    errorBundlePretty,
                                                    fancyFailure, many,
                                                    manyTill, noneOf, parse,
                                                    (<|>))
import           Text.Megaparsec.Char              (alphaNumChar, char,
                                                    digitChar, newline, space1,
                                                    string)
import qualified Text.Megaparsec.Char.Lexer        as L

-- External imports: auxiliary
import Data.ByteString.Extra as B (safeReadFile)

-- Internal imports: auxiliary
import Copilot.Core.Analysis       (exprIsConstant)
import Data.ExprPair               (ExprPair (..), ExprPairT (..))
import Language.Trans.SpecAnalysis (reifySpec)

-- * Diagram

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

-- | Diagram formats supported.
data DiagramFormat = Mermaid
                   | Dot
  deriving (Eq, Show)

-- * Diagram parsers

-- | Read a diagram from a file.
readDiagram :: FilePath                    -- ^ File containing diagram
            -> DiagramFormat               -- ^ Format of the input file
            -> ExprPair                    -- ^ Subparser for conditions or
                                           -- edge expressions
            -> IO (Either String Diagram)
readDiagram fp format exprP = do
  contentEither <- B.safeReadFile fp
  return $ do
    -- The following functions use Either to return errors. The use of (>>=) to
    -- chain functions makes the program stop at the earliest error.
    diagFileContent <- contentEither

    -- Abtract representation of a state machine diagram.
    parseDiagram format diagFileContent exprP

-- | Generic function to parse a diagram.
parseDiagram :: DiagramFormat          -- ^ Format of the input file
             -> B.ByteString           -- ^ Contents of the diagram
             -> ExprPair               -- ^ Subparser for conditions or edge
                                       -- expressions
             -> Either String Diagram
parseDiagram Dot     = parseDiagramDot
parseDiagram Mermaid = parseDiagramMermaid

-- ** Dot parser

-- | Parse a DOT / Graphviz diagram.
parseDiagramDot :: B.ByteString -> ExprPair -> Either String Diagram
parseDiagramDot contents exprP = do
    let contentsUTF8 = toUTF8 contents
    dg <- fst $ G.runParser G.parse $ preProcess contentsUTF8
    return $ makeDiagram dg
  where
    makeDiagram :: Gs.DotGraph LT.Text -> Diagram
    makeDiagram g = Diagram links
      where
        links = map edgeToLink (graphEdges g)

        edgeToLink edge =
            ( read (LT.unpack o)
            , exprPairShow exprP (LT.unpack e)
            , read (LT.unpack d)
            )
          where
            o = G.fromNode edge
            d = G.toNode edge
            e = getLabel (G.edgeAttributes edge)

            -- Extract the label from a list of attributes. If no label is
            -- found, it's assumed that the condition is the literal true.
            getLabel [] = "true"
            getLabel ((Attributes.Label (Attributes.StrLabel l)) : _) = l
            getLabel (_ : as) = getLabel as

-- ** Mermaid parser

-- | Parse a mermaid diagram.
parseDiagramMermaid :: B.ByteString -> ExprPair -> Either String Diagram
parseDiagramMermaid txtDia exprP =
    case parsingResult of
      Left e  -> Left (errorBundlePretty e)
      Right x -> Right x
  where
    txt           = T.decodeUtf8 (toStrict txtDia)
    parsingResult = parse (spaces *> pDiagram exprP) "<input>" txt

-- | Type for parser for memaid diagrams.
type MermaidParser = ParsecT Void Text Identity

-- | Parser for mermaid diagrams.
pDiagram :: ExprPair -> MermaidParser Diagram
pDiagram  exprP =
      pGraphDiagram exprP
  <|> pStateDiagram exprP
  <|> pSequenceDiagram exprP

-- | Parser for a mermaid diagram.
--
-- This parser depends on an auxiliary parser for the expressions associated to
-- the edges or connections between states.
pGraphDiagram :: ExprPair -> MermaidParser Diagram
pGraphDiagram exprP = do
  _ <- string "graph" <* spaces
  _name <- T.pack <$> manyTill alphaNumChar (char ';')
  _ <- newline

  transitions <- many (pGraphTransition exprP)

  pure $ Diagram transitions

-- | Parser for an edge in a state diagram.
--
-- This parser depends on an auxiliary parser for the expressions associated to
-- the edges or connections between states.
pGraphTransition :: ExprPair -> MermaidParser (Int, String, Int)
pGraphTransition ep@(ExprPair (ExprPairT { exprTParse = parseProp })) = do
  _ <- spaces
  stateFrom <- many digitChar
  _ <- string "-->|"
  edge <- many (noneOf ("|" :: [Char]))

  let x = parseProp edge
  when (isLeft x) $ fancyFailure $ Set.singleton $
    ErrorFail $ "Edge property has incorrect format: " ++ show edge

  _ <- char '|'
  stateTo <- many digitChar
  _ <- char ';'
  _ <- newline
  return (read stateFrom, exprPairShow ep edge, read stateTo)

-- | Parser for Mermaid diagrams of type stateDiagram-v2.
pStateDiagram :: ExprPair -> MermaidParser Diagram
pStateDiagram exprPair = do
  _ <- string "stateDiagram-v2" <* spaces

  transitions <- many (pStateTransition exprPair)

  pure $ Diagram transitions

-- | Parser for transition label in stateDiagram-v2 mermaid diagram.
pStateTransition :: ExprPair -> MermaidParser (Int, String, Int)
pStateTransition ep@(ExprPair (ExprPairT { exprTParse = parseProp })) = do
  _ <- spaces
  from <- read <$> many digitChar
  _ <- spaces
  string "-->"
  _ <- spaces
  to <- read <$> many digitChar
  _ <- spaces
  _ <- char ':'
  _ <- spaces
  edge <- many (noneOf ("\n" :: [Char]))

  let x = parseProp edge
  when (isLeft x) $ fancyFailure $ Set.singleton $
    ErrorFail $ "Edge property has incorrect format: " ++ show edge

  _ <- newline

  pure $ (from, exprPairShow ep edge, to)

-- | Parser for Mermaid diagrams of type sequenceDiagram.
pSequenceDiagram :: ExprPair -> MermaidParser Diagram
pSequenceDiagram exprPair = do
  spaces
  _ <- string "sequenceDiagram"
  spaces

  conditions <- many (pSequenceTransition exprPair)
  let transitions = zipWith (\t idx -> (idx, t, idx + 1)) conditions [0..]

  pure $ Diagram transitions

-- | Parser for a connection, message or transition in a sequence diagram.
--
-- This parser depends on an auxiliary parser for the expressions associated to
-- the connections or messages between elements.
pSequenceTransition :: ExprPair -> MermaidParser String
pSequenceTransition ep@(ExprPair (ExprPairT { exprTParse = parseProp })) = do
  spaces
  stateFrom <- many digitChar
  spaces
  pSequenceArrow
  spaces
  stateTo <- many digitChar
  spaces
  _ <- char ':'
  spaces
  edge <- many (noneOf ("\n" :: [Char]))

  let x = parseProp edge
  when (isLeft x) $ fancyFailure $ Set.singleton $
    ErrorFail $ "Edge property has incorrect format: " ++ show edge

  _ <- newline

  pure (exprPairShow ep edge)

-- | Parser for arrow in sequence diagram.
pSequenceArrow :: MermaidParser ()
pSequenceArrow = void $ choice
  [ string "->>"
  , string "-->>"
  , string "-)"
  ]

-- | Consume spaces
spaces :: MermaidParser ()
spaces = L.space space1 empty empty

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
  [ ("Control.Monad.Writer",  Nothing)
  , ("Copilot.Language",      Nothing)
  , ("Copilot.Language.Spec", Nothing)
  , ("Data.Functor.Identity", Nothing)
  , ("Data.List",             Just "L")
  , ("Prelude",               Just "P")
  ]

-- | Render a 'Diagram' as a Haskell definition of a 'Copilot.Spec'.
--
-- The shown 'Copilot.Spec' has a top-level triggers for the properties we are
-- interested in, as well as several auxiliary definitions.
showDiagram :: Diagram -> String
showDiagram diagram = unlines
    [ "do let"
    , ""
    , "       stateMachine :: (Eq a, Typed a)"
    , "                    => (a, a, Stream Bool, [(a, Stream Bool, a)], a)"
    , "                    -> Stream a"
    , "       stateMachine (initial, final, noInputData, transitions, bad) ="
    , "           state"
    , "         where"
    , "           state         = ifThenElses transitions"
    , "           previousState = [initial] ++ state"
    , ""
    , "           -- ifThenElses :: [(a, Stream Bool, a)] -> Stream a"
    , "           ifThenElses [] ="
    , "             ifThenElse"
    , "               (previousState == constant final && noInputData)"
    , "               (constant final)"
    , "               (constant bad)"
    , ""
    , "           ifThenElses ((s1, i, s2):ss) ="
    , "             ifThenElse"
    , "               (previousState == constant s1 && i)"
    , "               (constant s2)"
    , "               (ifThenElses ss)"
    , ""
    , "       isDeterministic :: (Ord a, Eq a, Typed a)"
    , "                       => (a, a, Stream Bool, [(a, Stream Bool, a)], a)"
    , "                       -> Stream Bool"
    , "       isDeterministic (_, _, _, ts, _) = all $"
    , "           map"
    , "             (\\s -> all (map not $ pairwise $ transitionsFrom s))"
    , "             states"
    , "         where"
    , "           states = L.nub $ L.sort $ concat $"
    , "             map (\\(s1, _, s2) -> [s1, s2]) ts"
    , ""
    , "           transitionsFrom s = [ t | (s1, t, _) <- ts, s P.== s1 ]"
    , ""
    , "       completeMachine :: (Ord a, Eq a, Typed a)"
    , "                       => (a, a, Stream Bool, [(a, Stream Bool, a)], a)"
    , "                       -> Stream Bool"
    , "       completeMachine (_, _, _, ts, _) = all $"
    , "           map (\\s -> or $ transitionsFrom s) states"
    , "         where"
    , "           states = L.nub $ L.sort $ concat $"
    , "             map (\\(s1, _, s2) -> [s1, s2]) ts"
    , "           transitionsFrom s = [ t | (s1, t, _) <- ts, s P.== s1 ]"
    , ""
    , "       all [] = true"
    , "       all (x:xs) = x && all xs"
    , ""
    , "       or [] = false"
    , "       or (x:xs) = x || or xs"
    , ""
    , "       pairwise :: [ Stream Bool ] -> [ Stream Bool ]"
    , "       pairwise []      = []"
    , "       pairwise (x:[])  = []"
    , "       pairwise (x1:xs) ="
    , "         (map (\\x2 -> (x1 && x2)) xs) P.++ pairwise xs"
    , ""
    , "       stateMachine1 :: ( Word8"
    , "                        , Word8"
    , "                        , Stream Bool"
    , "                        , [(Word8, Stream Bool, Word8)]"
    , "                        , Word8"
    , "                        )"
    , "       stateMachine1 ="
    , "         (initialState, finalState, noInput, transitions, badState)"
    , ""
    , "       initialState :: Word8"
    , "       initialState = " ++ show initialState
    , ""
    , "       finalState :: Word8"
    , "       finalState = " ++ show finalState
    , ""
    , "       noInput :: Stream Bool"
    , "       noInput = false"
    , ""
    , "       input :: Stream Word8"
    , "       input = extern \"input\" Nothing"
    , ""
    , "       badState :: Word8"
    , "       badState = " ++ show badState
    , ""
    , "       transitions = " ++ showTransitions
    , ""
    , "   trigger \"deterministic\" (isDeterministic stateMachine1) []"
    ]

  where

    -- Elements of the spec.
    initialState = minimum states
    finalState   = maximum states
    badState     = maximum states + 1

    -- States and transitions from the diagram.
    states      = diagramStates diagram
    transitions = diagramTransitions diagram

    showTransitions :: String
    showTransitions =
      "[" ++ intercalate ", " (map showTransition transitions) ++ "]"

    showTransition :: (Int, String, Int) -> String
    showTransition (a, b, c) =
      "(" ++ show a ++ ", " ++ b ++ ", " ++ show c ++ ")"

-- * Auxiliary functions

-- | Parse and print a value using an auxiliary Expression Pair.
--
-- Fails if the value has no valid parse.
exprPairShow :: ExprPair -> String -> String
exprPairShow (ExprPair exprP) =
    printProp . fromRight' . parseProp
  where
    ExprPairT parseProp _replace printProp _ids _unknown = exprP

-- | Unsafe fromRight. Fails if the value is a 'Left'.
fromRight' :: Either a b -> b
fromRight' (Right v) = v
fromRight' _         = error "fromRight' applied to Left value."
