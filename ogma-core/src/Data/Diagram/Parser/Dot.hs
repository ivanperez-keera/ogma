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
-- | Parsing of diagrams from Dot files.
module Data.Diagram.Parser.Dot
    ( parseDiagramDot
    )
  where

-- External imports
import qualified Data.ByteString.Lazy              as B
import           Data.GraphViz                     (graphEdges)
import qualified Data.GraphViz                     as G
import qualified Data.GraphViz.Attributes.Complete as Attributes
import           Data.GraphViz.Commands.IO         (toUTF8)
import qualified Data.GraphViz.Parsing             as G
import           Data.GraphViz.PreProcessing       (preProcess)
import qualified Data.GraphViz.Types.Generalised   as Gs
import qualified Data.Text.Lazy                    as LT

-- Internal imports: auxiliary
import Data.Diagram  (Diagram (..))
import Data.ExprPair (ExprPair, exprPairShow)

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
