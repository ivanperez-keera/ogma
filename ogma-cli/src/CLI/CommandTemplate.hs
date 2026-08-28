{-# LANGUAGE OverloadedStrings #-}
-- Copyright 2020 United States Government as represented by the Administrator
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
module CLI.CommandTemplate
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

-- * Command

-- | Options to generate an overview from the input specification(s).
data CommandOpts = CommandOpts
  { overviewProject    :: Maybe String
  , overviewInputFiles :: [TemplateFile]
  }

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc = "Handle application templates"

-- | Subparser for the @template@ command, used to handle application
-- templates.
commandOptsParser :: Parser CommandOpts
commandOptsParser = subparser
  ( subcommandTemplateCreate
  )

-- | Modifier for the create subcommand, linking the subcommand options and
-- description to the command @create@.
subcommandTemplateCreate:: Mod CommandFields CommandOpts
subcommandTemplateCreate =
  subcommand
    "create"
    (CommandOptsTemplateCreate<$> CLI.CommandTemplateCreate.commandOptsParser)
    CLI.CommandTemplateCreate.commandDesc
