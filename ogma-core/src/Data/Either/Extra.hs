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
-- | Auxiliary functions for working with values of type 'Either'.
module Data.Either.Extra
    ( makeLeft
    , mapLeft
    )
  where

-- | Replace the left value in an @Either@.
makeLeft :: c -> Either a b -> Either c b
makeLeft c (Left _)  = Left c
makeLeft _ (Right x) = Right x

-- | Apply a transformation only to the 'Left' values of an 'Either'.
--
-- Left counterpart of 'fmap'.
mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x)  = Left (f x)
mapLeft _ (Right x) = Right x
