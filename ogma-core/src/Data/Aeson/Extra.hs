-- Copyright 2022 United States Government as represented by the Administrator
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
-- | Auxiliary functions for working with JSON values.
module Data.Aeson.Extra
    ( mergeObjects )
  where

-- External imports
import Data.Aeson        (Value (Array, Null, Object))
import Data.Aeson.KeyMap (unionWith)

-- | Merge two JSON values.
--
-- Fails if the values are not objects or null.
mergeObjects :: Value -> Value -> Value
mergeObjects (Object m1) (Object m2) = Object $ unionWith mergeObjects m1 m2
mergeObjects (Array a)   (Array b)   = Array $ a <> b
mergeObjects obj         Null        = obj
mergeObjects Null        obj         = obj
mergeObjects a           _           = a
