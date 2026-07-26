{-# LANGUAGE DeriveGeneric #-}
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
-- | Ogma projects.
module Data.Project where

-- External imports
import           Control.Exception (IOException, try)
import           Data.Aeson        (FromJSON, ToJSON, eitherDecodeStrict')
import qualified Data.ByteString   as BS
import           GHC.Generics      (Generic)

-- Internal imports
import Data.Either.Extra (mapLeft)

data Project = Project
    { projectName           :: Maybe String
    , projectInputFiles     :: [(FilePath, String, String)]
                               -- ^ File, format name, prop name
    , projectVariableFiles  :: Maybe FilePath
    , projectVariableDBFile :: Maybe FilePath
    , projectHandlerFile    :: Maybe FilePath
    , projectCommandPropVia :: Maybe FilePath
    , projectTemplateDir    :: Maybe FilePath
    , projectTargetDir      :: Maybe FilePath
    , projectExtraJSONFile  :: Maybe FilePath
    }
  deriving (Generic, Show)

instance FromJSON Project
instance ToJSON Project

-- | Read a project from a file.
readProject :: FilePath -> IO (Either String Project)
readProject path = do
  bytesResult <- try (BS.readFile path)
  pure $ case bytesResult of
           Left e      -> Left (show (e :: IOException))
           Right bytes -> mapLeft ("Failed to read project: " ++)
                        $ eitherDecodeStrict' bytes
