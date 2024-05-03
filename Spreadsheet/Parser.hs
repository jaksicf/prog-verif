{-# LANGUAGE ViewPatterns #-}

------------
-- PARSER --
------------

-- Here we define a parser for spreadsheets.

module Spreadsheet.Parser (
  pcellpos,
  pexprfull,
  pcell,
  psheet,
) where

import Data.Char (ord, isDigit)
import Text.Read (readMaybe)

import Spreadsheet.Ast
import Spreadsheet.Lexer
import Spreadsheet.Tokens
import Spreadsheet.Utils
import Debug.Trace (trace)

-- Parse a type.
ptype :: TokenTree -> Type
ptype (Keyword _ KInt) = Int
ptype (Keyword _ KBool) = Bool
ptype tt = tterr tt "expected type"

-- Parse a variable declaration (without the `var` keyword).
parg :: TokenStream -> (VarDecl, TokenStream)
parg ((Ident _ ident) : (Operator _ ":") : ty : rest) = ((ident, ptype ty), rest)
parg [] = error "expected variable declaration"
parg (tt : rest) = tterr tt "unexpected token in variable declaration"

-- Parse a single identifer.
pident :: TokenStream -> (String, TokenStream)
pident ((Ident _ ident) : rest) = (ident, rest)
pident [] = error "expected variable name"
pident (tt : rest) = tterr tt "unexpected token in variable name"

-- Combinator for comma-separated lists.
commasep :: (TokenStream -> (a, TokenStream)) -> TokenStream -> [a]
commasep _ [] = []
commasep parse rest = v : (after srest)
  where
    (v, srest) = parse rest
    after ((Operator _ ",") : rest) = commasep parse rest
    after [] = []
    after (tt : rest) = tterr tt "expected comma"

-- We use it to construct parsers for expression lists, argument lists, and
-- identifier lists:
pexprlist  = commasep pexpr
parglist   = commasep parg
pidentlist = commasep pident

-- Parse a full expression. The token stream must be fully consumed once the
-- expression is parsed.
pexprfull :: TokenStream -> Expr

pexprfull rest = checkDone expr srest
  where
    (expr, srest) = pexpr rest
    checkDone expr [] = expr
    checkDone expr (tt : rest) = tterr tt "unexpected token after expression"

-- Defines unary operators.
unop :: String -> Bool
unop "!" = True
unop "-" = True
unop _   = False

pcellcol :: Char -> Maybe Int
pcellcol c = if 'A' <= c && c <= 'Z'
  then Just (ord c - ord 'A')
  else Nothing

pcellrow :: String -> Maybe Int
pcellrow c = readMaybe c

-- EXTENDED IMPLEMENTATION, to support cells like AA1, AAAZ3, etc.
excelColToInt :: String -> Int
excelColToInt excelCol = res
  where
    getColComponentForCharAtPosition position c = checkColChar c (((ord c) - (ord 'A') + 1) * (26 ^ position))
    summed = sum $ zipWith getColComponentForCharAtPosition [0..] (reverse excelCol)
    res = summed - 1
    -- check that each char is in the valid range
    checkColChar c other = if 'A' <= c && c <= 'Z' then other else error "wrong column char used, must be A-Z"

pcellpos :: String -> CellPos
pcellpos cellPos =  (colPos, rowPos-1)
  where
    (colStr, rowStr) = break isDigit cellPos
    colPos = excelColToInt colStr
    Just rowPos = pcellrow rowStr

-- OLD IMPLEMENTATION:
-- pcellpos ((pcellcol -> Just col) : (pcellrow -> Just row)) = (col, row - 1)
-- pcellpos _ = error "invalid cell reference"

-- Parse an expression at the beginning of a token stream, then return the
-- remaining (not-yet-parsed) tokens.
pexpr :: TokenStream -> (Expr, TokenStream)
pexpr ((Group _ '(' expr) : rest) = pexprnext (EParens (pexprfull expr)) rest
pexpr ((Literal _ (LBool v)) : rest) = pexprnext (EConstBool v) rest
pexpr ((Literal _ (LInt v)) : rest) = pexprnext (EConstInt v) rest
pexpr ((Ident _ func) : (Group _ '(' args) : rest) = pexprnext (ECall func (pexprlist args)) rest
pexpr ((Keyword _ KCell) : (Ident _ pos) : rest) = pexprnext (ECell (pcellpos pos)) rest
pexpr ((Keyword _ KRange) : (Ident _ pos1) : (Ident _ pos2) : rest) = pexprnext (ERange (pcellpos pos1) (pcellpos pos2)) rest
pexpr ((Ident _ ident) : rest) = pexprnext (EVar ident) rest
pexpr ((Operator _ op@(unop -> True)) : rest) = ((unop expr), srest)
  where
    (expr, srest) = pexpr rest

    -- Make a unary operation, pushing into the LHS of any binary operations
    -- as needed. (Unary operators have a higher precedence than any binary
    -- operator in this language.)
    unop (EBinaryOp lhs bop rhs) = EBinaryOp (unop lhs) bop rhs
    unop e = EUnaryOp op e

pexpr [] = error "expected expression"
pexpr (tt : _) = tterr tt "unexpected token in expression"

-- Defines the precedence of binary operators.
binprec :: String -> Maybe Int
binprec "%"  = Just 1
binprec "*"  = Just 2
binprec "/"  = Just 2
binprec "+"  = Just 3
binprec "-"  = Just 3
binprec "<"  = Just 4
binprec ">"  = Just 4
binprec "!=" = Just 4
binprec "==" = Just 4
binprec "<=" = Just 4
binprec ">=" = Just 4
binprec "&&" = Just 5
binprec "||" = Just 5
binprec "=>" = Just 6
binprec _    = Nothing

-- Given a `base` expression, parse additional expressions which may follow it,
-- namely to form binary operations with the `base` expression as the left-hand
-- side. Resolves precedence of binary operations using precedence climbing.
pexprnext :: Expr -> TokenStream -> (Expr, TokenStream)
pexprnext lhs ((Operator _ op@(binprec -> Just prec)) : rest) = (binop lhs op rhs, srest)
  where
    (rhs, srest) = pexpr rest

    -- Make a binary operation, swapping operands around if necessary to
    -- preserve operator precedence.
    binop lhs op1 rhs@(EBinaryOp rhs1 op2 rhs2)
      | swap op1 op2 = EBinaryOp (binop lhs op1 rhs1) op2 rhs2
      | otherwise    = EBinaryOp lhs op1 rhs
    binop lhs op rhs = EBinaryOp lhs op rhs

    -- Should a swap happen?
    swap op1 op2 = prec1 <= prec2
      where
        Just prec1 = binprec op1
        Just prec2 = binprec op2
pexprnext base rest = (base, rest)

-- Parse a (deterministic) conditional statement. This handles the case of
-- `elif` by recursively parsing the second conditional.
pif :: TokenStream -> (Stmt, TokenStream)
pif ((Keyword _ KIf) : (Group _ '(' cond) : (Group _ '{' codeIf) : rest) =
  (Cond (pexprfull cond) (pcode codeIf) selse, srest)
  where
    (selse, srest) = pelseifs rest
    pelseifs :: TokenStream -> (Code, TokenStream)
    pelseifs ((Keyword _ KElif) : (Group _ '(' cond) : (Group _ '{' codeElif) : rest)
      = ([Cond (pexprfull cond) (pcode codeElif) eelse], erest)
      where (eelse, erest) = pelseifs rest
    pelseifs (tt@(Keyword _ KElif) : rest) = tterr tt "malformed conditional statement"
    pelseifs ((Keyword _ KElse) : (Group _ '{' codeElse) : rest) = (pcode codeElse, rest)
    pelseifs (tt@(Keyword _ KElse) : rest) = tterr tt "malformed conditional statement"
    pelseifs rest = ([], rest)
pif (tt : rest) = tterr tt "malformed conditional statement"

-- Parse a code block.
pcode :: TokenStream -> Code
pcode ((Ident _ name) : (Operator _ ":=") : rest) =
  (Assign name expr) : (pcode srest)
  where (expr, srest) = pexpr rest
pcode ((Keyword _ KIf) : (Operator _ "*") : (Group _ '{' codeIf) : (Keyword _ KElse) : (Group _ '{' codeElse) : rest) =
  (Nondet (pcode codeIf) (pcode codeElse)) : (pcode rest)
pcode ((Keyword _ KIf) : (Operator _ "*") : (Group _ '{' codeIf) : rest) =
  (Nondet (pcode codeIf) [Skip]) : (pcode rest)
pcode src@((Keyword _ KIf) : _) = ifStmt : (pcode rest)
  where (ifStmt, rest) = pif src

--     var (name): (type) := (expr)
pcode ((Keyword _ KVar) : (Ident _ name) : (Operator _ ":") : ty : (Operator _ ":=") : rest) =
  (Local name (ptype ty) (Just expr)) : (pcode srest)
  where (expr, srest) = pexpr rest
--     var (name): (type)
pcode ((Keyword _ KVar) : (Ident _ name) : (Operator _ ":") : ty : rest) =
  (Local name (ptype ty) Nothing) : (pcode rest)
--     assert (expr)
pcode ((Keyword _ KAssert) : rest) =
  (Assert expr) : (pcode srest)
  where (expr, srest) = pexpr rest
--     return (expr)
pcode ((Keyword _ KReturn) : rest) =
  (Return expr) : (pcode srest)
  where (expr, srest) = pexpr rest

pcode [] = []
pcode (tt : rest) = tterr tt "unexpected token"


-- EXTRA: parse row() cells
pRowCellArgs :: [TokenTree] -> Cell
pRowCellArgs args =
  let parsedArgs = pexprlist args
  in case parsedArgs of
    [EConstInt len, init, op] -> CRow len init op
    _ -> error "Error: missing args for row() cell"

-- Parse a cell.
pcell :: TokenStream -> Cell
pcell [] = CEmpty
pcell [Keyword _ (KComment _)] = CEmpty
pcell [Keyword _ KConst, Literal _ (LInt v)] = CConst v
pcell ((Keyword p KConst) : _) = err p "invalid constant"
pcell [Keyword _ KInput] = CInput Nothing
pcell ((Keyword p KInput) : rest) = CInput (Just (pexprfull rest))

-- EXTRA: parse row() cells
pcell [Keyword _ KRow, Group _ '(' args] = pRowCellArgs args

pcell [Keyword _ KProgram, Group _ '{' program] = CProgram (pcode program) Nothing False
pcell ((Keyword _ KProgram) : (Group _ '{' program) : (Keyword _ KTransparent) : rest) = CProgram (pcode program) Nothing True
pcell ((Keyword _ KProgram) : (Group _ '{' program) : (Keyword _ KEnsures) : rest) = CProgram (pcode program) (Just (pexprfull rest)) False
pcell ((Keyword p KProgram) : _) = err p "invalid program"

-- Parse a spreadsheet.
psheet :: [[TokenStream]] -> Spreadsheet
psheet = (map . map) pcell
