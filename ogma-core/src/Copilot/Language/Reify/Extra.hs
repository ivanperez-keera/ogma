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

-- | Typechecking of Copilot specs.
module Copilot.Language.Reify.Extra
    ( reifySpec )
  where

-- External imports
import qualified Copilot.Core                 as Core
import qualified Copilot.Language             as Copilot
import qualified Copilot.Language.Reify       as Copilot
import qualified Language.Haskell.Interpreter as HI

-- | Read a specification from a 'String' and reify it.
--
-- This function receives a list of possibly qualified imports.
reifySpec :: [(String, Maybe String)] -> String -> IO Core.Spec
reifySpec imports specText = do
  coreSpecE <- HI.runInterpreter $ do
                 HI.setImportsQ imports
                 copilotSpec <- HI.interpret specText (HI.as :: Copilot.Spec)
                 HI.liftIO $ Copilot.reify copilotSpec

  case coreSpecE of
    Left err -> do putStrLn $ "Error: " ++ show err
                   error $ show err

    Right coreSpec -> return coreSpec
