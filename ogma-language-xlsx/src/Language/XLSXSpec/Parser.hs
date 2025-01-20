{-# LANGUAGE ScopedTypeVariables #-}
-- Copyright 2024 United States Government as represented by the Administrator
-- of the National Aeronautics and Space Administration. All Rights Reserved.
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

-- | Parser for Ogma specs stored in XLSX files.
module Language.XLSXSpec.Parser where

-- External imports
import           Control.Monad           (forM, sequence)
import Codec.Xlsx
import Data.List (lookup)
import Data.Maybe (fromJust, isNothing, catMaybes)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Vector             as V
import qualified Data.ByteString.Lazy as L

-- External imports: ogma-spec
import Data.OgmaSpec (Requirement (..), Spec (Spec))

-- | List of XPath routes to the elements we need to parse.
--
-- The optional paths denote elements that may not exist. If they are nothing,
-- those elements are not parsed in the input file.
--
-- The subfields are applied on each string matching the parent element. That
-- is, the internal var ID XPath will be a applied to the strings returned when
-- applying the internal vars XPath (if it exists). Paths whose names are
-- plural denote expected lists of elements.
--
-- The components of a tuple (String, Maybe (String, String)) mean the
-- following: if a string is present but the second component is Nothing, then
-- the string is the XPath expression to be used. If a Just value is present,
-- the first element of its inner tuple represents a key, and the second
-- element represents an XPath expression that will produce a value when
-- evaluated globally in the file. After evaluating that expression, the key
-- must be found in the first string of the three and replaced with the result
-- of evaluating the expression.
data XLSXFormat = XLSXFormat
    { skipHeaders          :: Bool
    , specRequirementSheet :: String
    , specRequirementId    :: Int
    , specRequirementDesc  :: Maybe Int
    , specRequirementExpr  :: Int
    }
  deriving (Show, Read)

-- | Parse a XLSX file and extract a Spec from it.
--
-- An auxiliary function must be provided to parse the requirement expressions.
--
-- Fails if any of the columns indicate a column out of range, if the XLSX is
-- malformed.
parseXLSXSpec :: (String -> IO (Either String a)) -- ^ Parser for expressions.
             -> a
             -> XLSXFormat                        -- ^ Format for spec locations.
             -> L.ByteString                     -- ^ String containing XLSX
             -> IO (Either String (Spec a))
parseXLSXSpec parseExpr _defA xlsxFormat value = do
  let bsToString = T.unpack . T.decodeUtf8
      stringToBS = TL.encodeUtf8 . TL.pack

  let internalVariableDefs = []
      externalVariableDefs = []

  let sheets = _xlSheets <$> toXlsxEither value

  case sheets of
      Left err -> return $ Left $ parseErrorMsg err
      Right sheets'  -> do
        let sheet  = fromJust
                   $ lookup (T.pack (specRequirementSheet xlsxFormat)) sheets'
            cells  = _wsCells sheet
            rows   = toRows cells

            cellFromRow i row = cellValueToString
                              $ fromJust
                              $ _cellValue
                              $ fromJust
                              $ lookup (ColumnIndex i) row

            emptyCell i row = isNothing (lookup (ColumnIndex i) row)

            emptyRow row =  emptyCell (specRequirementExpr xlsxFormat) row
                         || emptyCell (specRequirementId   xlsxFormat) row
                         || maybe False (`emptyCell` row) (specRequirementDesc xlsxFormat)

        let rows' = if skipHeaders xlsxFormat then tail rows else rows

        rs <- forM rows' $ \(_, row) -> do
          print row
          if emptyRow row
             then return $ Right $ Nothing
             else do
               expr <- parseExpr $
                         cellFromRow (specRequirementExpr xlsxFormat) row
               case expr of
                 Left  e -> return $ Left $ "The XLSX data could not be parsed: " ++ e
                 Right e -> return $ Right $ Just
                   Requirement
                     { requirementName =
                         cellFromRow (specRequirementId xlsxFormat) row
                     , requirementDescription =
                         maybe "" ((`cellFromRow` row)) $
                           (specRequirementDesc xlsxFormat)
                     , requirementExpr = e
                     }

        let rs'  = sequence rs
            rs'' = fmap catMaybes rs'

        case rs'' of
          Left err    -> return $ Left err
          Right rs''' -> return $ Right $ Spec
                                           internalVariableDefs
                                           externalVariableDefs
                                           rs'''

parseErrorMsg :: ParseError -> String
parseErrorMsg (InvalidZipArchive string) = "Invalid zip archive: " ++ string
parseErrorMsg (MissingFile fp) = "Missing file: " ++ fp
parseErrorMsg (InvalidFile fp txt) = "Invalid file: " ++ fp
parseErrorMsg (InvalidRef fp refId) = "Invalid reference in file: " ++ fp
parseErrorMsg (InconsistentXlsx txt) = "Inconsistent XLSX file"

cellValueToString :: CellValue -> String
cellValueToString (CellText txt) = T.unpack txt
cellValueToString (CellDouble n) = show n
cellValueToString (CellBool b) = show b
cellValueToString (CellRich _) = "(unsupported)"
cellValueToString (CellError _) = "(error)"
