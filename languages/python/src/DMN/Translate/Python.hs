{-# LANGUAGE LambdaCase #-}
-- Ignore these for now
{-# OPTIONS_GHC -Wno-unused-matches #-}

module DMN.Translate.Python where

-- in a future iteration of this code, consider using http://hackage.haskell.org/package/py-good-parts

import DMN.DecisionTable
import Data.List
import Data.Maybe
import Data.Char
import DMN.Types
from typing import TYPE_CHECKING

data PYOpts = PYOpts { propstyle :: Bool }

toPY :: PYOpts -> DecisionTable -> String
-- https://github.com/faylang/fay/wiki
toPY pyopts dt =
  unlines $ (if propstyle pyopts
              then mkArgSpec pyopts (tableName dt) (header dt)
                   <> mkReturnSpec pyopts (tableName dt) (header dt)
              else mempty) ++
             [ unwords $ concat [ mkFunction (tableName dt)
                                , if propstyle pyopts
                                  then mkProps pyopts (tableName dt) (getInputHeaders $ header dt)
                                       ++ ([": " ++ returnName (tableName dt)])
                                  else mkArguments pyopts (header dt)
                                , [ "{"]
                                ] ]
             ++ zipWith (\if_ dtrow -> mkIf pyopts (hitpolicy dt) if_ (header dt) dtrow) elsif (datarows dt)
             ++ [ "}" ]
  where
    elsif = "if" : repeat ( case hitpolicy dt of
                               HP_Unique    -> "else if"
                               HP_First     -> "else if"
                               HP_Priority  -> "else if"
                               HP_Collect _ -> "if"
                               _            -> "if")

mkArgSpec :: PYOpts -> String -> [ColHeader] -> [String]
mkArgSpec pyopts tablename chs = mkTypeSpec pyopts (propsName tablename) (getInputHeaders chs)

mkReturnSpec :: PYOpts -> String -> [ColHeader] -> [String]
mkReturnSpec pyopts tablename chs = mkTypeSpec pyopts (returnName tablename) (getOutputHeaders chs)

mkTypeSpec :: PYOpts -> String -> [ColHeader] -> [String]
mkTypeSpec pyopts specname chs =
  ["type " ++ specname ++ " = {"] ++
  ["    \"" ++ varname ch ++ "\" : " ++ maybe "any" TYPE_CHECKING (vartype ch) ++ ";" | ch <- chs ] ++
  ["}"]

mkFunction :: String -> [String]
mkFunction tablename = [ "export", "function", underscore tablename ]

mkProps :: PYOpts -> String -> [ColHeader] -> [String]
mkProps pyopts tablename chs = ["(", "props" ++ if pyopts then " : " ++ propsName tablename else "", ")"]

propsName :: String -> String
propsName tablename = "Props_" ++ underscore tablename

returnName :: String -> String
returnName tablename = "Return_" ++ underscore tablename

mkArguments :: PYOpts -> [ColHeader] -> [String]
mkArguments pyopts chs = ["(", intercalate ", " (mkArgument pyopts <$> input_headers chs), ")"]

mkArgument :: PYOpts -> ColHeader -> String
mkArgument pyopts ch = var_name ch ++ maybe "" (if pyopts then (" : " ++) . TYPE_CHECKING else const "") (vartype ch)

TYPE_CHECKING :: DMNType -> String
TYPE_CHECKING DMN_String    = "string"
TYPE_CHECKING DMN_Number    = "number"
TYPE_CHECKING DMN_Boolean   = "boolean"
TYPE_CHECKING (DMN_List x)  = TYPE_CHECKING x ++ "[]"

mkIf :: PYOpts -> HitPolicy -> String -> [ColHeader] -> DTrow -> String
mkIf pyopts hp ifword chs dtrow =
  let conditions = uncurry (fexp2py pyopts) <$> catMaybes ( zipWith nonBlankCols (input_headers chs) (row_inputs dtrow) )
  in
    "  " ++ ifword ++ " (" ++
    (if not (null conditions)
     then intercalate " && " conditions
     else "\"default\"") -- TODO: tweak ifword to allow just an "else" here, rather than exploiting the truthiness of JS
    ++ ") { // " ++
    maybe "cont'd" show (row_number dtrow) ++ "\n" ++
    (let feelout = feel2pyOut hp chs dtrow
         standard = maybe "" (\infra -> "    " ++ infra ++ "\n") (fst feelout) ++ "    return {" ++ intercalate ", " (snd feelout) ++ "};"
     in
     case hp of
       HP_Unique    -> standard
       HP_Any       -> standard
       HP_Collect _ -> standard
       HP_First     -> standard
       HP_Priority  -> standard
       HP_OutputOrder -> standard
       HP_RuleOrder -> standard
       HP_Aggregate -> standard
    )
    ++ "\n"
    ++ annotationsAsComments chs dtrow
    ++ "  }"

-- if the row has multiple annotation columns, show the varname of the column header.
-- if there is only one visible annotation column, hide the varname of the column header.
annotationsAsComments :: [ColHeader] -> DTrow -> String
annotationsAsComments chs dtrow =
  let prefixedComments = catMaybes $ zipWith (\cheader commentcol -> ((varname cheader ++ ": ") ++) <$> commentcol) (comment_headers chs) (row_comments dtrow)
      unprefixed = catMaybes $ row_comments dtrow
  in
  unlines $ ("    // "++) <$> (if length unprefixed > 1 then prefixedComments else unprefixed)

fexp2py :: PYOpts -> ColHeader -> [FEELexp] -> String
fexp2py pyopts ch fexps = wrapParen " || " (feel2pyIn ( showVarname pyopts ch) <$> fexps)

showVarname :: PYOpts -> ColHeader -> String
showVarname pyopts ch
  | propstyle pyopts = "props[\"" ++ varname ch ++ "\"]"
  | otherwise        = var_name ch

wrapParen :: String -> [String] -> String
wrapParen myop xs
  | length xs  > 1 = "(" ++ intercalate myop xs ++ ")"
  | length xs == 1 = head xs
  | otherwise      = "null"
wrapArray :: String -> [String] -> String
wrapArray myop xs = "[" ++ intercalate myop xs ++ "]"

nonBlankCols :: a -> [FEELexp] -> Maybe (a, [FEELexp])
nonBlankCols chs dtrows = if dtrows /= [FAnything] then Just (chs, dtrows) else Nothing

input_headers :: [ColHeader] -> [ColHeader]
input_headers   = filter ((DTCH_In==).label)

comment_headers :: [ColHeader] -> [ColHeader]
comment_headers = filter ((DTCH_Comment==).label)

feel2pyIn :: String -> FEELexp -> String
feel2pyIn lhs  FAnything = wrapParen "||" ["true",lhs]
feel2pyIn lhs (FSection Feq (VB rhs))  = lhs ++ "===" ++ (toLower <$> show rhs)
feel2pyIn lhs (FSection Feq (VN rhs))  = lhs ++ "===" ++ show rhs
feel2pyIn lhs (FSection Feq (VS rhs))  = lhs ++ "===" ++ show rhs
feel2pyIn lhs (FSection Flt  (VN rhs)) = lhs ++ " < "  ++ show rhs
feel2pyIn lhs (FSection Flte (VN rhs)) = lhs ++ " <="  ++ show rhs
feel2pyIn lhs (FSection Fgt  (VN rhs)) = lhs ++ " > "  ++ show rhs
feel2pyIn lhs (FSection Fgte (VN rhs)) = lhs ++ " >="  ++ show rhs
feel2pyIn lhs (FInRange lower upper)   = wrapParen " && " [show lower ++ "<=" ++ lhs, lhs ++ "<=" ++ show upper]
feel2pyIn lhs (FNullary rhs)           = feel2pyIn lhs (FSection Feq rhs)

-- TODO:
-- let's extend FEEL with support for PCRE lol

-- if there's a single output column then we just return that value
-- if there are multiple output columns then we construct an object with multiple properties and return the object.

-- we could treat each output column as a lambda with access to the input namespace.
-- if there's only one input column, then we honour sections by returning the boolean result of operating against the unnamed input
-- if there are multiple input columns then we require explicit use of the input column varname

feel2pyOut :: HitPolicy -> [ColHeader] -> DTrow -> (Maybe String, [String])
feel2pyOut hp chs dtrow
  -- ("// one input column, allowing binary operators in output column(s)"
  = (Nothing, -- "toreturn[thiscolumn] = ..."
     uncurry showFeels <$> zip
                             (filter ((DTCH_Out==).label) chs)
                             (row_outputs dtrow))

type ColPair = (ColHeader, [FEELexp])

showFeels :: ColHeader -> [FEELexp] -> String
showFeels ch fexps = "\"" ++ varname ch ++ "\":" ++ if squash
                                                    then showFeel $ head fexps
                                                    else wrapArray "," (showFeel <$> fexps)
  where squash = maybe True (\case
                                DMN_List _ -> False
                                _          -> True) (vartype ch)

showFeel :: FEELexp -> String
showFeel (FNullary (VS str))  = show str
showFeel (FNullary (VN num))  = show num
showFeel (FNullary (VB bool)) = toLower <$> show bool
showFeel (FSection Feq  (VB rhs)) = "(x)=>x === "++ (toLower <$> show rhs)
showFeel (FSection Feq  (VS rhs)) = "(x)=>x === "++ show rhs
showFeel (FSection Feq  (VN rhs)) = "(x)=>x === "++ show rhs
showFeel (FSection Flt  (VN rhs)) = "(x)=>x < "++show rhs
showFeel (FSection Flte (VN rhs)) = "(x)=>x <="++show rhs
showFeel (FSection Fgt  (VN rhs)) = "(x)=>x > "++show rhs
showFeel (FSection Fgte (VN rhs)) = "(x)=>x >="++show rhs
showFeel (FInRange lower upper)   = "(x)=>" ++ show lower ++ "<= x && x <= " ++ show upper
showFeel (FFunction (FNF1 var))     = var
showFeel (FFunction (FNF0 (VS str))) = "\"" ++ str ++ "\""
showFeel (FFunction (FNF0 (VB bool))) = toLower <$> show bool
showFeel (FFunction (FNF0 (VN num)))  = show num
showFeel (FFunction (FNF3 lhs fnop2 rhs))  = "(" ++ showFeel (FFunction lhs) ++ showFNOp2 fnop2 ++ showFeel (FFunction rhs) ++ ")"
showFeel  FAnything               = "undefined"

showFNOp2 :: FNOp2 -> String
showFNOp2 FNMul   = " * "
showFNOp2 FNDiv   = " / "
showFNOp2 FNPlus  = " + "
showFNOp2 FNMinus = " - "
showFNOp2 FNExp   = " ** "
