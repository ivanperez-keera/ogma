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

-- | Formally analyze Copilot Core specifications.
module Copilot.Core.Analysis
    ( exprIsConstant )
  where

-- External imports
import qualified Copilot.Core          as Core
import           Copilot.Theorem.What4 (SatResult (..), Solver (Z3), prove)
import           Data.List             (lookup)

-- | Determine if a boolean expression is always 'True' or always 'False'.
--
-- The first boolean in the result is 'True' if the expression can be proven
-- always 'True'. The second boolean in the expression is 'True' is the
-- expression can be proven always 'False'.
--
-- They values in the tuple cannot both 'True' at the same time.
exprIsConstant :: Core.Spec
               -> Core.Name
               -> Core.Expr Bool
               -> IO (Bool, Bool)
exprIsConstant spec name expr = do
  r1 <- propIsValid spec name (Core.Forall expr)
  r2 <- propIsValid spec name (Core.Forall (Core.Op1 Core.Not expr))
  pure (r1, r2)

-- | 'True' if the Copilot 'Prop' with the given name and expression is
-- constantly 'True', or valid, and 'False' otherwise (not always 'True' or
-- unknown).
propIsValid :: Core.Spec
            -> Core.Name
            -> Core.Prop
            -> IO Bool
propIsValid spec name expr =
    maybe False isValid . lookup name <$> prove Z3 spec'
  where
    spec' = spec { Core.specProperties = prop' : Core.specProperties spec }
    prop' = Core.Property name expr

    isValid :: SatResult -> Bool
    isValid Valid = True
    isValid _     = False
