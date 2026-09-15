-- Copyright 2026 United States Government as represented by the Administrator
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
-- | CLI interface to the Template subcommand.
module CLI.CommandTemplateCreate
    (
      -- * Direct command access
      command
    , CommandOpts
    , ErrorCode

      -- * CLI
    , commandDesc
    , commandOptsParser
    )
  where

-- External imports
import Options.Applicative ( Parser, help, long, many, metavar, optional,
                             short, showDefault, strOption, value )

-- External imports: handling of command results
import Command.Result ( Result(..) )

import qualified Command.TemplateCreate

-- * Command

-- | Options to manipulate templates.
data CommandOpts = CommandOpts
  { templateCreateSource      :: FilePath
  , templateCreateReplacement :: FilePath
  , templateCreateTargetDir   :: FilePath
  }

-- | Print an overview of the input specification(s).
command :: CommandOpts -> IO (Result ErrorCode)
command c = Command.TemplateCreate.command $ Command.TemplateCreate.CommandOpts
  (templateCreateSource c)
  (templateCreateReplacement c)
  (templateCreateTargetDir c)

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc = "Create an application template"

-- | Subparser for the @template create@ command, used to create a template by
-- applying a list of substitutions to a file or directory.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts
  <$> strOption
        (  long "input-path"
        <> short 'i'
        <> metavar "FILEPATH"
        <> help strTemplateCreateInputPathDesc
        )
  <*> strOption
        (  long "replace-with"
        <> short 'r'
        <> metavar "FILENAME"
        <> help strTemplateCreateReplaceWithDesc
        )
  <*> strOption
        (  long "target-dir"
        <> metavar "DIRNAME"
        <> help strTemplateCreateTargetDirDesc
        <> showDefault
        <> value "."
        )

-- | Input path flag description.
strTemplateCreateInputPathDesc :: String
strTemplateCreateInputPathDesc =
  "Input file or directory to be create a template from"

-- | Replacement file flag description.
strTemplateCreateReplaceWithDesc :: String
strTemplateCreateReplaceWithDesc =
  "JSON file containing a list of replacements to apply"

-- | Target dir flag description.
strTemplateCreateTargetDirDesc :: String
strTemplateCreateTargetDirDesc =
  "Target directory where the template should be created"

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error, 2 for internal bug.
type ErrorCode = Int
