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
-- | CLI interface to the Overview subcommand.
module CLI.CommandOverview
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
import           Data.Aeson          (toJSON)
import           Data.List           (dropWhileEnd)
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.IO   as T
import           Options.Applicative (Parser, help, long, metavar, optional,
                                      short, showDefault, some, strOption,
                                      value)
import           Text.Microstache

-- External imports: command results
import Command.Result ( Result(..) )

-- External imports: actions or commands supported
import           Command.Overview (ErrorCode)
import qualified Command.Overview

-- * Command

-- | Options to generate an overview from the input specification(s).
data CommandOpts = CommandOpts
  { overviewInputFiles :: [OverviewFile]
  }

-- | Options associated to a specific input file.
data OverviewFile = OverviewFile
  { overviewFilePath       :: FilePath
  , overviewFileFormat     :: String
  , overviewFilePropFormat :: String
  , overviewFilePropVia    :: Maybe String
  }

-- | Print an overview of the input specification(s).
command :: CommandOpts -> IO (Result ErrorCode)
command c = do
    (mOutput, result) <-
      Command.Overview.command internalCommandOpts

    case mOutput of
      Just output ->
        case outputString of
          Right template ->
            T.putStr $ trimEnd $ renderMustache template (toJSON output)
          _              -> putStrLn "Error"
      _ -> putStrLn "Error"
    return result

  where

    trimEnd :: T.Text -> T.Text
    trimEnd = T.unlines . dropWhileEnd T.null . T.lines

    internalCommandOpts :: Command.Overview.CommandOptions
    internalCommandOpts = Command.Overview.CommandOptions $
      map fileInfo (overviewInputFiles c)

    fileInfo f = Command.Overview.OverviewFile
      { Command.Overview.overviewFilePath       = overviewFilePath   f
      , Command.Overview.overviewFileFormat     = overviewFileFormat f
      , Command.Overview.overviewFilePropFormat = overviewFilePropFormat f
      , Command.Overview.overviewFilePropVia    = overviewFilePropVia f
      }

    outputString =
      compileMustacheText "output" $ T.unlines
        [ "{{#commandSummaryRequirements}}"
        , "The requirements file {{commandRequirementsFile}} has:"
        , " - {{commandExternalVariables}} external variables."
        , " - {{commandInternalVariables}} internal variables."
        , " - {{commandRequirements}} requirements."
        , "   - {{commandRequirementsTrue}} requirements are constantly or always true."
        , "   - {{commandRequirementsFalse}} requirements are constantly or always false."
        , "{{#commandRequirementsConsistent}}"
        , "   - No inconsistencies detected in the requirements."
        , "{{/commandRequirementsConsistent}}"
        , "{{^commandRequirementsConsistent}}"
        , "   - The requirements are not mutually consistent."
        , "{{/commandRequirementsConsistent}}"
        , ""
        , "{{/commandSummaryRequirements}}"
        , "{{#commandSummaryDiagrams}}"
        , "The diagram file {{commandDiagramFile}}:"
        , " - Has {{commandDiagramNumStates}} states."
        , "{{#commandDiagramDeterministic}}"
        , " - Is deterministic."
        , "{{/commandDiagramDeterministic}}"
        , "{{^commandDiagramDeterministic}}"
        , " - Is not deterministic."
        , "{{/commandDiagramDeterministic}}"
        , ""
        , "{{/commandSummaryDiagrams}}"
        ]

-- * CLI

-- | Command description for CLI help.
commandDesc :: String
commandDesc = "Generate an overview of the input specification(s)"

-- | Subparser for the @overview@ command, used to generate an overview
-- of the input specifications.
commandOptsParser :: Parser CommandOpts
commandOptsParser = CommandOpts <$> some overviewFileOptsParser

-- | Subparser for information on one input file to be used with the @overview@
-- command.
overviewFileOptsParser :: Parser OverviewFile
overviewFileOptsParser = OverviewFile
  <$> strOption
        (  long "input-file"
        <> metavar "FILENAME"
        <> help strOverviewInputFileDesc
        )
  <*> strOption
        (  long "input-format"
        <> short 'f'
        <> metavar "FORMAT_NAME"
        <> help strOverviewFormatDesc
        <> showDefault
        <> value "fcs"
        )
  <*> strOption
        (  long "prop-format"
        <> short 'p'
        <> metavar "FORMAT_NAME"
        <> help strOverviewPropFormatDesc
        <> showDefault
        <> value "smv"
        )
  <*> optional
        ( strOption
            (  long "parse-prop-via"
            <> metavar "COMMAND"
            <> help strOverviewPropViaDesc
            )
        )

-- | Input file flag description.
strOverviewInputFileDesc :: String
strOverviewInputFileDesc = "File with properties or requirements"

-- | Format flag description.
strOverviewFormatDesc :: String
strOverviewFormatDesc = "Format of the input file"

-- | Property format flag description.
strOverviewPropFormatDesc :: String
strOverviewPropFormatDesc = "Format of temporal or boolean properties"

-- | External command to pre-process individual properties.
strOverviewPropViaDesc :: String
strOverviewPropViaDesc =
  "Command to pre-process individual properties"
