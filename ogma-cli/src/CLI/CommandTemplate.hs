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

-- External imports
import           Options.Applicative ( CommandFields, Mod, Parser, helper,
                                       info, progDesc, subparser, (<**>) )
import qualified Options.Applicative as OptParse

-- External imports: command results
import Command.Result ( Result )

-- Internal imports
import qualified CLI.CommandTemplateCreate

-- * Command

-- * Command dispatcher

-- | Command dispatcher that obtains the parameters from the command line and
-- passes them as arguments to the actual function that will process them,
-- transforming the local result into a global program result.

-- This function is implemented as a combination of three processes or
-- functions: one that adapts the input to the inner function (down), the
-- actual function implementing the command, and a translation of the local
-- result into a global result that can be reported to users (up). In this
-- case, the commands are all using the same (and compatible) exit codes, but
-- they might not do so. This is captured by the fmap of the function id on
-- each processing command.
--
-- The function that adapts the inputs simply passes the arguments obtained
-- from the command line one by one. Composition with this function uncurries
-- the next function to work over the product as defined by the record that
-- accompanies each command definition in the type OgmaCLICommand.
--
-- Neither this nor the internal commands not know, and need to know, that they
-- run in CLI.
command :: CommandOpts -> IO (Result ErrorCode)
command (CommandOptsTemplateCreate c) =
  id <$> CLI.CommandTemplateCreate.command c

-- | Options to manipulate templates.
data CommandOpts =
  CommandOptsTemplateCreate CLI.CommandTemplateCreate.CommandOpts

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
    (CommandOptsTemplateCreate <$> CLI.CommandTemplateCreate.commandOptsParser)
    CLI.CommandTemplateCreate.commandDesc

-- * Error codes

-- | Encoding of reasons why the command can fail.
--
-- The error code used is 1 for user error, 2 for internal bug.
type ErrorCode = Int

-- * Auxiliary defs

-- | Build a subcommand modifier from a command name, option parser and command
-- description.
subcommand :: String
           -> Parser CommandOpts
           -> String
           -> Mod CommandFields CommandOpts
subcommand entry parser desc =
  OptParse.command entry (info (parser <**> helper) (progDesc desc))
