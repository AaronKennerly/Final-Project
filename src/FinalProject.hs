module FinalProject where

import Data.Char
import Data.List
import Data.Fixed

type Name = String
type Vars = String
type Value = Double
type Env = [(Vars,Value)]

data AExpr = Var Vars | Const Value | Add AExpr AExpr | Mul AExpr AExpr
           | Sub AExpr AExpr | Div AExpr AExpr | Mod AExpr AExpr
           | Exp AExpr AExpr
  deriving Show

--           'G'    'Q'   'P'   'C'   'F'
data LOps = GalOp | QOp | POp | COp | FOp
  deriving Show

--          '$'      'E'      'K'        'D'
data MOps = DolOp | EuroOp | PoundOp | PesosOP
  deriving Show

data BOps = AddOp | MulOp | SubOp | DivOp | ModOp | ExpOp
  deriving Show
data Token = VSym String | CSym Double 
           | BOp BOps | LPar | RPar | Comma | Dot
           | PA AExpr | PList [AExpr]
           | Err String | AssignOp | QuitK
           | LConvertK | MConvertK | LOp LOps | MOp MOps
  deriving Show


-- PEMDAS
alevel :: BOps -> Integer
alevel ExpOp = 100
alevel MulOp = 80
alevel DivOp = 60
alevel ModOp = 50
alevel AddOp = 40
alevel SubOp = 20


-- This section is for AExpr
----------------------------------------------------------------------------------------------
-- This function will call the lexer then the parser on the result
readAExpr :: String -> AExpr
readAExpr = parseAExpr . lexer

-- This is the parser for AExpr 
parseAExpr :: [Token] -> AExpr
parseAExpr s = case sr [] s of
  [PA e]  -> e
  [Err e] -> error $ "Lexical error: " ++ e
  s       -> error $ "Parse error: " ++ show s

sr :: [Token] -> [Token] -> [Token]
-- reduce phase
sr (VSym x : s) i                    = sr (PA (Var x) : s)     i -- AExpr -> Var x
sr (CSym n : s) i                    = sr (PA (Const n) : s)   i -- AExpr -> Const n
sr t@(PA e2 : BOp op1 : PA e1 : s) (BOp op2 : i) | alevel op2 > alevel op1
                                     = sr (BOp op2 : t) i
sr (PA e2 : BOp AddOp : PA e1 : s) i = sr (PA (Add e1 e2) : s) i -- AExpr -> AExpr + AExpr
sr (PA e2 : BOp MulOp : PA e1 : s) i = sr (PA (Mul e1 e2) : s) i -- AExpr -> AExpr * AExpr
sr (PA e2 : BOp SubOp : PA e1 : s) i = sr (PA (Sub e1 e2) : s) i -- AExpr -> AExpr - AExpr
sr (PA e2 : BOp DivOp : PA e1 : s) i = sr (PA (Div e1 e2) : s) i -- AExpr -> AExpr / AExpr
sr (PA e2 : BOp ModOp : PA e1 : s) i = sr (PA (Mod e1 e2) : s) i -- AExpr -> AExpr % AExpr
sr (PA e2 : BOp ExpOp : PA e1 : s) i = sr (PA (Exp e1 e2) : s) i -- AExpr -> AExpr ^ AExpr
sr (RPar : PA e : LPar : s) i        = sr (PA e : s) i           -- AExpr -> ( AExpr )
-- shift phase
sr s (i:is) = sr (i:s) is
-- base case
-- sr [PA e] []                         = e
sr (Err e : s) i = [Err e]
sr s [] = s


-- This is the lexer for AExpr
lexer :: String -> [Token]
lexer "" = []
lexer xs | isPrefixOf "quit" xs = QuitK : lexer (drop 4 xs)                        -- the quit
lexer (x:xs) | isLower x = let (hd,tl) = span (\x -> isAlphaNum x || x == '_') xs  -- the vars
                           in VSym  (x:hd) : lexer tl
lexer (c:cs) | isDigit c = case span isDigit (c:cs) of                             -- the doubles
                  (x,'.':xs) -> case span isDigit xs of
                              ([], ys) -> [Err (x++'.':xs)]
                              (y, ys) -> CSym (read (x ++ "." ++ y)) : lexer ys 
                  (x, xs) -> let (hd,tl) = span isDigit xs
                              in CSym (read $ x++hd) : lexer tl
lexer (x:xs) | isDigit x = let (hd,tl) = span isDigit xs                           -- the ints
                           in CSym (read $ x:hd) : lexer tl
lexer ('.':x:xs) | isDigit x = let (hd,tl) = span isDigit xs                       -- the 0. doubles
                           in CSym (read $ "0." ++ x:hd) : lexer tl
lexer ('.':s) = [Err s]
lexer ('+':xs) = BOp AddOp : lexer xs
lexer ('*':xs) = BOp MulOp : lexer xs
lexer ('-':xs) = BOp SubOp : lexer xs
lexer ('/':xs) = BOp DivOp : lexer xs
lexer ('%':xs) = BOp ModOp : lexer xs
lexer ('^':xs) = BOp ExpOp : lexer xs
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer (':':'=':xs) = AssignOp : lexer xs
lexer (x:xs) | isSpace x = lexer xs
lexer s = [Err s]


-- This is the eval for Aexpr
eval :: Env -> AExpr -> Value
eval env (Var x) = maybe (error "Variable not found") id (lookup x env)
eval env (Const n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 / eval env e2
eval env (Mod e1 e2) = eval env e1 `mod'` eval env e2
eval env (Exp e1 e2) = eval env e1 ** eval env e2

----------------------------------------------------------------------------------------------

-- This section is for the conversion types
----------------------------------------------------------------------------------------------
-- liqud converter Lexer
lexCon :: String -> [Token]
lexCon "" = []
lexCon (c:cs) | isDigit c = case span isDigit (c:cs) of
                  (x,'.':xs) -> case span isDigit xs of
                              (y, ys) -> CSym (read (x ++ "." ++ y)) : lexCon ys
                  (x, _) -> [CSym (read x)]
lexCon (x:xs) | isDigit x = let (hd,tl) = span isDigit xs
                           in CSym (read $ x:hd) : lexCon tl
lexCon ('.':x:xs) | isDigit x = let (hd,tl) = span isDigit xs
                           in CSym (read $ "0." ++ x:hd) : lexCon tl
lexCon ('G':xs) = LOp GalOp : lexCon xs
lexCon ('Q':xs) = LOp QOp : lexCon xs
lexCon ('P':xs) = LOp POp : lexCon xs
lexCon ('C':xs) = LOp COp : lexCon xs
lexCon ('F':xs) = LOp FOp : lexCon xs
lexCon ('$':xs) = MOp DolOp : lexCon xs
lexCon ('E':xs) = MOp EuroOp : lexCon xs
lexCon ('K':xs) = MOp PoundOp : lexCon xs
lexCon ('D':xs) = MOp PesosOP : lexCon xs
lexCon (x:xs) | isSpace x = lexCon xs
lexCon s = [Err s]

-- LConverter - returns [Gallon value, quart value, pint vale, cup value, fluid ounce value]
lConvert :: [Token] -> String
lConvert [] = ""
lConvert (Err s : xs) = "Convert error: " ++ s
lConvert  (LOp GalOp : CSym x : xs) =
  "Gallon " ++ show x ++ ", Quart " ++ show (x*4) ++ ", Pint " ++ show (x*8) ++
  ", Cup " ++ show (x*16) ++ ", Fl Oz " ++ show (x*128)
lConvert (LOp QOp : CSym x : xs) =
  "Gallon " ++ show (x/4) ++ ", Quart " ++ show x ++ ", Pint " ++ show (x*2) ++
  ", Cup " ++ show (x*4) ++ ", Fl Oz " ++ show (x*32)
lConvert (LOp POp : CSym x : xs) =
  "Gallon " ++ show x ++ ", Quart " ++ show (x*2) ++ ", Pint " ++ show x ++
  ", Cup " ++ show (x/2) ++ ", Fl Oz " ++ show (x*16)
lConvert (LOp COp : CSym x : xs) =
  "Gallon " ++ show (x/4) ++ ", Quart " ++ show x ++ ", Pint " ++ show (x*2) ++
  ", Cup " ++ show (x*4) ++ ", Fl Oz " ++ show (x*32)
lConvert (LOp FOp : CSym x : xs) =
  "Gallon " ++ show x ++ ", Quart " ++ show (x*4) ++ ", Pint " ++ show (x*8) ++
  ", Cup " ++ show (x*16) ++ ", Fl Oz " ++ show (x*128)
lConvert _ = "Convert error!"



-- MConverter - returns [Dollar value, Pound value, Euro vale, Peso value]
mConvert :: [Token] -> String
mConvert [] = ""
mConvert (Err s : xs) = "Convert error: " ++ s
mConvert (MOp DolOp : CSym x : xs) = 
  "Dollar " ++ show x ++ ", Pound " ++ show (x*0.79) ++ 
  ", Euro " ++ show (x*0.9) ++ ", Pesos " ++ show (x*17.97)
mConvert (MOp PoundOp : CSym x : xs) = 
  "Dollar " ++ show (x*1.26) ++ ", Pound " ++ show x ++
  ", Euro " ++ show (x*1.14) ++ ", Pesos " ++ show (x*22.53)
mConvert (MOp EuroOp : CSym x : xs) =
  "Dollar " ++ show (x*1.1) ++ ", Pound " ++ show (x*0.86) ++
  ", Euro " ++ show x ++ ", Pesos " ++ show (x*19.84)
mConvert (MOp PesosOP : CSym x : xs) =
  "Dollar " ++ show (x*0.056) ++ ", Pound " ++ show (x*0.044) ++
  ", Euro " ++ show (x*0.050) ++ ", Pesos " ++ show x
mConvert _ = "Convert error!"
----------------------------------------------------------------------------------------------


-- read-eval-print loop
repl :: Env -> String -> String
repl env input = do
  let lexed = lexer input
  case lexed of
    (VSym x : AssignOp : ts) -> do
      let parsedTokens = parseAExpr ts
      let newval = eval env parsedTokens
      repl ((x,newval):env) ""
    ts -> do
      let parsed = parseAExpr lexed
      let evaled = eval env parsed
      let output = show evaled
      output
