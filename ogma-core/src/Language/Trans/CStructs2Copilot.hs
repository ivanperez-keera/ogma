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
-- | Generate Copilot struct definitions and instances from structs defined in
-- a C header file.
--
-- Working with Copilot structs requires three definitions: the datatype,
-- a @Struct@ instance, and a @Typed@ instance.
--
-- This module converts the C structs into 'CStruct's, and then converts
-- those 'CStruct's into Copilot (i.e., Haskell) data type declarations and
-- instance declarations represented as strings.
module Language.Trans.CStructs2Copilot where

-- External imports
import Data.Char ( isUpper, toLower )
import Data.List ( intercalate )

-- External imports: auxiliary
import Data.List.Extra ( toHead, toTail )

-- Internal imports: C AST
import qualified Language.C.AbsC          as C
import           Language.Copilot.CStruct ( CField (CArray, CPlain),
                                            CStruct (..) )

-- Internal imports: Copilot's representation of C structs
import Language.Trans.CStruct2CopilotStruct ( camelCaseTypeName, mkCStruct )

-- | Convert all the 'CStruct's in a header file into the declarations needed
-- in Copilot to use it.
cstructs2CopilotDecls :: C.TranslationUnit -> Either String [ String ]
cstructs2CopilotDecls (C.MkTranslationUnit gs) =
  concat <$> mapM (fmap cstruct2CopilotDecls  . mkCStruct) gs

-- | Convert a 'CStruct' into the declarations needed in Copilot to use it.
cstruct2CopilotDecls :: CStruct -> [ String ]
cstruct2CopilotDecls cstruct = [ cStructToCopilotStruct cstruct
                               , structInstance         cstruct
                               , typedInstance          cstruct
                               ]

-- ** Individual conversions

-- | Convert a 'CStruct' definition into a Copilot Struct declaration.
--
-- For example, given the struct generated by the following definition:
--
-- @
--   struct {
--     uint8_t f1;
--   } a_struct_t;
-- @
--
-- the corresponding Haskell definition would be:
--
-- @
-- data AStruct = AStruct
--   { aSF1 :: Word8 }
-- @
cStructToCopilotStruct :: CStruct -> String
cStructToCopilotStruct cstruct =
    "data "
    ++ datatype
    ++ " = "
    ++ constructor
  where

    -- The name of the type (e.g., @AStruct@).
    datatype = cStructName2Haskell (cStructName cstruct)

    -- The name of the constructor (e.g., @AStruct@).
    constructor = cStructName2Haskell (cStructName cstruct) ++ "\n" ++ fields

    -- The fields in the struct (e.g., @aSF1 :: Word 8@), formated as record
    -- fields: separated by commas, enclosed in curly brackets, and indented.
    fields = unlines $ map ("  " ++)
                     $ (++ ["}"])
                     $ toTail (", " ++)
                     $ toHead ("{ " ++)
                     $ map (toField cstruct) (cStructFields cstruct)

    -- Convert a 'CStruct' field into a Copilot record field declaration.
    --
    -- The second case (@CArray@) uses depedent types to promote the length of
    -- the array to type level.
    toField :: CStruct -> CField -> String
    toField cstruct' (CPlain t n) = name ++ " :: " ++ ty
      where
        name = fieldName cstruct' n
        ty   = "Field" ++ " " ++ show n
                       ++ " " ++ cTypeName2HaskellType t

    toField cstruct' (CArray t n l) = name ++ " :: " ++ ty
      where
        name = fieldName cstruct' n
        ty   = "Field" ++ " " ++ show n
                       ++ " (" ++ "Array"
                       ++ " " ++ show l
                       ++ " " ++ cTypeName2HaskellType t
                       ++ ")"

-- | Convert a 'CStruct' definition into a Copilot @Struct@ instance
-- declaration. For example, for the struct:
--
-- @
--   struct {
--     uint8_t f1;
--   } a_struct_t;
-- @
--
-- the corresponding @Struct@ instance would be:
--
-- @
--   instance Struct AStruct where
--     typename _ = "a_struct_t"
--     toValues v = [Value Word8 (aSF1 v)]
-- @
structInstance :: CStruct -> String
structInstance cstruct =
    "instance " ++ instanceHead ++ " where\n" ++
    unlines (map ("  " ++ ) instanceBody)
  where
    instanceHead = "Struct" ++ " " ++ instanceName
    instanceName = cStructName2Haskell $ cStructName cstruct

    instanceBody = [ instanceTypeName, instanceToValues ]

    instanceTypeName = "typeName" ++ " " ++ "_" ++ " = " ++ show (cStructName cstruct)

    instanceToValues =
      "toValues" ++ " " ++ "v" ++ " = " ++ "[ " ++ intercalate ", " valueDecls ++ " ]"

    valueDecls = map (toValueDecl cstruct) (cStructFields cstruct)

    toValueDecl :: CStruct -> CField -> String
    toValueDecl c (CPlain t n) =
      "Value" ++ " " ++ cTypeName2HaskellType t
              ++ " (" ++ fieldName c n
              ++ " " ++ "v" ++ ")"

    toValueDecl c (CArray t n _len) =
      "Value" ++ " (" ++ "Array"
              ++ " " ++ cTypeName2HaskellType t ++ ")"
              ++ " " ++ "(" ++ fieldName c n
              ++ " " ++ "v" ++ ")"

-- | Convert a 'CStruct' definition to Copilot @Typed@ instance declaration.
-- For example, for the struct:
--
-- @
--   struct {
--     uint8_t f1;
--   } a_struct_t;
-- @
--
-- the corresponding @Typed@ instance could be:
--
-- @
--   instance Typed AStruct where
--     typeOf = Struct (Field 0)
-- @
typedInstance :: CStruct -> String
typedInstance cstruct =
    "instance " ++ instanceHead ++ " where\n" ++
    unlines (map ("  " ++ ) instanceBody)
  where
    instanceHead = "Typed" ++ " " ++ instanceName
    instanceName = cStructName2Haskell $ cStructName cstruct

    instanceBody   = [ instanceTypeOf ]

    instanceTypeOf = "typeOf" ++ " = " ++ "Struct" ++ " "
                  ++ "(" ++ dataConstructorName ++ " " ++ unwords vs ++ ")"

    dataConstructorName = cStructName2Haskell $ cStructName cstruct

    vs = map ((\x -> "(" ++ "Field" ++ " " ++ x ++ ")") . defaultValue)
             (cStructFields cstruct)

-- * Auxiliary functions

-- | Provide a suitable field name for a record field of a 'CStruct' in Haskell.
--
-- For example, given the struct:
--
-- @
--   struct {
--     uint8_t f1;
--   } a_struct_t;
-- @
--
-- the field name in the Haskell record would be @aSF1@, where the @aS@ and
-- comes from @a_struct_t@ and the final @F1@ comes from @f1@.
fieldName :: CStruct -> String -> String
fieldName cstruct n =
    summary (cStructName2Haskell (cStructName cstruct)) ++ cStructName2Haskell n
  where
    summary :: String -> String
    summary = map toLower . filter isUpper

-- | Convert a C struct name (e.g., @some_type_t@) to a Haskell type name
-- (e.g., @SomeType@).
cStructName2Haskell :: String -> String
cStructName2Haskell = camelCaseTypeName

-- | Return the corresponding type in Copilot/Haskell for a given type.
cTypeName2HaskellType :: String -> String
cTypeName2HaskellType "float"    = "Float"
cTypeName2HaskellType "double"   = "Double"
cTypeName2HaskellType "int"      = "Int"
cTypeName2HaskellType "uint8_t"  = "Word8"
cTypeName2HaskellType "uint16_t" = "Word16"
cTypeName2HaskellType "uint32_t" = "Word32"
cTypeName2HaskellType "uint64_t" = "Word64"
cTypeName2HaskellType "int8_t"   = "Int8"
cTypeName2HaskellType "int16_t"  = "Int16"
cTypeName2HaskellType "int32_t"  = "Int32"
cTypeName2HaskellType "int64_t"  = "Int64"
cTypeName2HaskellType "bool"     = "Bool"
cTypeName2HaskellType t          = camelCaseTypeName t

-- | Create a default value in Copilot for a field of a given type.
defaultValue :: CField -> String
defaultValue CArray {}    = "(array [])"
defaultValue (CPlain t _) = defaultValueOfType t
  where
    defaultValueOfType "float"    = "0"
    defaultValueOfType "double"   = "0"
    defaultValueOfType "int"      = "0"
    defaultValueOfType "uint8_t"  = "0"
    defaultValueOfType "uint16_t" = "0"
    defaultValueOfType "uint32_t" = "0"
    defaultValueOfType "uint64_t" = "0"
    defaultValueOfType "int8_t"   = "0"
    defaultValueOfType "int16_t"  = "0"
    defaultValueOfType "int32_t"  = "0"
    defaultValueOfType "int64_t"  = "0"
    defaultValueOfType "bool"     = "True"
    defaultValueOfType _t         = "0"
