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
-- | Parsing of diagrams.
module Data.Diagram.Parser
    ( DiagramFormat(..)
    , readDiagram
    )
  where

-- External imports
import qualified Data.ByteString.Lazy as B

-- External imports: auxiliary
import Data.ByteString.Extra as B (safeReadFile)

-- Internal imports: auxiliary
import Data.Diagram                (Diagram)
import Data.Diagram.Parser.Dot     (parseDiagramDot)
import Data.Diagram.Parser.Mermaid (parseDiagramMermaid)
import Data.ExprPair               (ExprPair)

-- | Diagram formats supported.
data DiagramFormat = Mermaid
                   | Dot
  deriving (Eq, Show)

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

--- | Generic function to parse a diagram.
parseDiagram :: DiagramFormat          -- ^ Format of the input file
             -> B.ByteString           -- ^ Contents of the diagram
             -> ExprPair               -- ^ Subparser for conditions or edge
                                       -- expressions
             -> Either String Diagram
parseDiagram Dot     = parseDiagramDot
parseDiagram Mermaid = parseDiagramMermaid
