-- Copyright 2020 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
--
-- Disclaimers
--
-- No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY
-- OF ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT
-- LIMITED TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO
-- SPECIFICATIONS, ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
-- PARTICULAR PURPOSE, OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE
-- SUBJECT SOFTWARE WILL BE ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF
-- PROVIDED, WILL CONFORM TO THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN
-- ANY MANNER, CONSTITUTE AN ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR
-- RECIPIENT OF ANY RESULTS, RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR
-- ANY OTHER APPLICATIONS RESULTING FROM USE OF THE SUBJECT SOFTWARE. FURTHER,
-- GOVERNMENT AGENCY DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING
-- THIRD-PARTY SOFTWARE, IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES
-- IT "AS IS." 
--
-- Waiver and Indemnity: RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST
-- THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS
-- ANY PRIOR RECIPIENT. IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS IN
-- ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH USE,
-- INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM, RECIPIENT'S
-- USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD HARMLESS THE
-- UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL AS ANY
-- PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW. RECIPIENT'S SOLE REMEDY
-- FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE, UNILATERAL TERMINATION OF THIS
-- AGREEMENT.
--
-- | Test CoCoSpec language library.
module Main where

-- External imports
import Data.Either                          ( isLeft, isRight )
import System.Filepath                      ( (</>) )
import Test.Framework                       ( Test, defaultMainWithOpts )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.QuickCheck                      ( Property )
import Test.QuickCheck.Monadic              ( assert, monadicIO, run )

-- Internal imports
import qualified Language.CoCoSpec.ParCoCoSpec as CoCoSpec ( myLexer,
                                                             pBoolSpec )
import           Paths_ogma_language_cocospec  ( getDataDir )

-- | Run all unit tests for the CoCoSpec parser.
main :: IO ()
main =
  defaultMainWithOpts tests mempty

-- | All unit tests for the CoCoSpec parser.
tests :: [Test.Framework.Test]
tests =
  [ testProperty "Parse CoCoSpec (correct case)"   propParseCoCoSpecOk
  , testProperty "Parse CoCoSpec (incorrect case)" propParseCoCoSpecFail
  ]

-- | Test the CoCoSpec parser on a well-formed boolean specification.
propParseCoCoSpecOk :: Property
propParseCoCoSpecOk = monadicIO $ do
  -- Get the auxiliary files from the files included with the cabal package
  dataDir <- getDataDir
  let filePath = dataDir </> "tests/cocospec_good"

  content <- run $ readFile filePath
  let program = CoCoSpec.pBoolSpec $ CoCoSpec.myLexer content
  assert (isRight program)

-- | Test the CoCoSpec parser on an incorrect boolean specification.
propParseCoCoSpecFail :: Property
propParseCoCoSpecFail = monadicIO $ do
  -- Get the auxiliary files from the files included with the cabal package
  dataDir <- getDataDir
  let filePath = dataDir </> "tests/cocospec_bad"

  content <- run $ readFile filePath
  let program = CoCoSpec.pBoolSpec $ CoCoSpec.myLexer content
  assert (isLeft program)
