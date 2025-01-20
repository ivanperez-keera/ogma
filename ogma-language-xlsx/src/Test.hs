import Language.XMLSpec.Parser

test fp = do
  f <- readFile fp
  parseXMLSpec (\x -> return (Right x))
               ""
               format
               f

format =
  XMLFormat
    { specInternalVars    = Nothing
    , specInternalVarId   = ("//*", Nothing)
    , specInternalVarExpr = ("//*", Nothing)
    , specInternalVarType = Nothing
    , specExternalVars    = Nothing
    , specExternalVarId   = ("//*", Nothing)
    , specExternalVarType = Nothing
    , specRequirements    = ("//Requirement", Nothing)
    , specRequirementId   = ("//Requirement/@Id/text()", Nothing)
    , specRequirementDesc = Just ("//Requirement/@Id/text()", Nothing)
    , specRequirementExpr = ("//Requirement/@Text/text()", Nothing)
    }
