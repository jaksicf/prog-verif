-------------
-- ENCODER --
-------------

-- This is the (skeleton of the) spreadsheet to Viper encoder; you will need to
-- implement the `encodeexpr`, `encodeprogram`, and `encode` function in this
-- module.

module Encoder.Encoder (
  encodeexpr,
  encodeprogram,
  encode,
) where

import Data.Char (chr)
import Data.List (sort, nub)

import Spreadsheet.Ast

-- Viper AST types are available as `VProgram`, `VExpr`, etc.
import Viper.Ast

encodeprogram :: Cell -> VMember
encodeprogram cell = head (makeCellFunc 0 0 cell)


-- Cells can depend on other cells, so encode them in a graph like way to construct the
-- Viper prog. Like from leaf to root cells, where leaf cells are cells without a
-- dependency.
encode :: Spreadsheet -> VProgram
encode sheet = VProgram  cellFunctions preludeString
  where
    preludeString = ""
    cellFunctions = makeCellFunctions sheet


-- WIP: just encode each cell as it's own method
makeCellFunctions :: [[Cell]] -> [VMember]
makeCellFunctions sheet = concatMapWithIndex concatRow sheet
  where
    concatRow rowIndex row = concatMapWithIndex (\colIndex cell -> makeCellFunc rowIndex colIndex cell) row


makeCellFunc :: Int -> Int -> Cell -> [VMember]
makeCellFunc rowNo colNo CEmpty = []
-- 📝 input
makeCellFunc rowNo colNo (CInput assumedExprCell) =
  let argsDecl = []
      returnsDecl = [("value", VSimpleType "Int")]
      requiresExpr = []
      funcName = getCellName colNo rowNo
      assumedExprViper = case assumedExprCell of
        Just expression -> [encodeexpr expression]
        Nothing -> []
  in case assumedExprCell of
  Nothing         -> [VMethod funcName argsDecl returnsDecl requiresExpr []          (Just (VSeq statements))]
    where
      statements = [VComment "📝 input cell"]
  Just expression -> [VMethod funcName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
    where
      ensuresExpr = [encodeexpr expression]
      statements = [
        VComment "📝 input cell"
        , VAssume (encodeexpr expression)]

-- #️⃣ const
makeCellFunc rowNo colNo (CConst cellValue) = [VMethod funcName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
  where
    argsDecl = []
    returnsDecl = [("value", VSimpleType "Int")]
    requiresExpr = []
    ensuresExpr = [VBinaryOp (VVar "value") "==" (VIntLit (toInteger cellValue))]
    statements = [VComment "#️⃣ const cell",
            VVarAssign "value" (VIntLit (toInteger cellValue))]
    funcName = getCellName colNo rowNo

-- 💻 program
makeCellFunc rowNo colNo (CProgram code postcond isTransp) = [VMethod funcName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
  where
    argsDecl = []
    returnsDecl = [("value", VSimpleType "Int")]
    requiresExpr = []
    ensuresExpr = case postcond of
      Just expr -> [encodeexpr expr]
      Nothing -> []
    statements = [VComment "💻 program cell"] ++ encodeCode code
    funcName = getCellName colNo rowNo


encodeexpr :: Expr -> VExpr
encodeexpr expr = case expr of
  EConstInt value -> VIntLit (toInteger value)             -- integer constant
  EBinaryOp subExpr1 op subExpr2 -> VBinaryOp (encodeexpr subExpr1) op (encodeexpr subExpr2) -- binary operation
  EVar variable -> VVar variable      -- global or local variable
  ECell (col, row) -> VVar ("__" ++ getCellName col row)
  EUnaryOp op expr -> VUnaryOp op (encodeexpr expr)      -- unary operation
  EParens expr -> encodeexpr expr               -- expression grouped in parentheses
  -- EXTRA:
  EConstBool bool -> if bool then VTrueLit else VFalseLit            -- true, false
  _ -> error "NOT IMPLEMENTED YET"
  -- TODO:
  -- EXTRA: | ECall String [Expr]
  -- EXTRA: | ERange CellPos CellPos

encodeCode :: [Stmt] -> [VStmt]
encodeCode code = hoistedLocals ++ [VComment "-----HOISTED----"] ++ (encodeCodeSub code) ++ [VLabel "__end"]
  where
    cellVars = findCellVarsInCode code
    hoistedLocals = genLocalsForCells cellVars

encodeCodeSub code = map encodeStmt code
  where
    encodeStmt stmt = case stmt of
      Skip                -> VComment "Skip stmtm was here"   -- no-op
      Assign varName expr -> VVarAssign varName (encodeexpr expr) -- assignment to variable
      Cond expr ifCode elseCode -> VIf (encodeexpr expr) (VSeq (encodeCodeSub ifCode)) (VSeq (encodeCodeSub elseCode)) -- conditional; note that `elif` is represented as  another conditional in the `else` branch
      Assert expr         -> VAssert (encodeexpr expr)   -- assertion
      Local varName varType Nothing     -> VSeq [VVarDecl varName (VSimpleType (show varType)), VVarAssign varName (if varType == Int then VIntLit 0 else VFalseLit)] -- local variable declaration
      Local varName varType (Just expr) -> VSeq [VVarDecl varName (VSimpleType (show varType)), VVarAssign varName (encodeexpr expr)] -- local variable declaration
      Return expr         -> VSeq [VVarAssign "value" (encodeexpr expr), VGoto "__end"]
      Nondet _ _          -> error "non-deterministic choice (not used in this project!)"


-- HELPERS
genLocalsForCells :: [(Int, Int)] -> [VStmt]
genLocalsForCells cells = map genLocalForCel cells
  where
    genLocalForCel (col, row) = VSeq [VVarDecl varName (VSimpleType "Int"), VVarAssign varName (VFuncApp (getCellName col row) [])]
      where
      varName = "__" ++ (getCellName col row)

findCellVarsInCode :: [Stmt] -> [(Int, Int)]
findCellVarsInCode code = removeDuplicates (concatMap findCellVarsInStmt code)
  where
    findCellVarsInStmt stmt = case stmt of
      Skip                -> []
      Assign varName expr -> findCellVarsInExpr expr -- assignment to variable
      Cond expr ifCode elseCode -> (findCellVarsInExpr expr) ++ (findCellVarsInCode ifCode) ++ (findCellVarsInCode elseCode)
      Assert expr         -> findCellVarsInExpr expr   -- assertion
      Local varName varType Nothing -> []
      Local varName varType (Just expr) -> findCellVarsInExpr expr
      Return expr         -> findCellVarsInExpr expr
      Nondet _ _          -> error "non-deterministic choice (not used in this project!)"
    findCellVarsInExpr expr = case expr of
      ECell (col, row) -> [(col, row)]
      EBinaryOp expr1 op expr2 -> (findCellVarsInExpr expr1) ++ (findCellVarsInExpr expr2) -- binary operation
      EVar variable -> []      -- global or local variable
      EConstInt value -> []             -- integer constant
      _ -> []
      -- TODO:
      -- | EParens Expr               -- expression grouped in parentheses
      -- | EUnaryOp String Expr       -- unary operation


removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = nub . sort


getCellName :: Int -> Int -> String
getCellName colNo rowNo = (intToAscii (colNo+65)) ++ show (rowNo + 1)

concatMapWithIndex :: (Int -> a -> [b]) -> [a] -> [b]
concatMapWithIndex f xs = concat $ zipWith f [0..] xs

intToAscii :: Int -> String
intToAscii n = [chr n]
