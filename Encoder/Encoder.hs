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

import Spreadsheet.Ast

-- Viper AST types are available as `VProgram`, `VExpr`, etc.
import Viper.Ast

encodeprogram :: Cell -> VMember
encodeprogram cell = head (makeCellFunc 0 0 cell)


-- Cells can depend on other cells, so encode them in a graph like way to construct the
-- Viper prog. Like from leaf to root cells, where leaf cells are cells without a
-- dependency.
encode :: Spreadsheet -> VProgram
encode sheet = VProgram (mainMethod : cellFunctions) preludeString
  where
    preludeString = ""
    cellFunctions = makeCellFunctions sheet
    mainMethod = VMethod "main" argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))
      where
        argsDecl = []
        returnsDecl = []
        requiresExpr = []
        ensuresExpr = []
        statements = [VComment "my amazing comment"]


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
makeCellFunc rowNo colNo (CConst cellValue) = makeViperMethod rowNo colNo body
  where
    body = [VComment "#️⃣ const cell",
            VVarAssign "value" (VIntLit (toInteger cellValue))]

-- TODO
makeCellFunc rowNo colNo (CProgram code postcond isTransp) = makeViperMethod rowNo colNo body
  where
    body = [VComment "❓ other cell"]


makeViperMethod :: Int -> Int -> [VStmt] -> [VMember]
makeViperMethod rowNo colNo body = [VMethod funcName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
  where
    argsDecl = []
    returnsDecl = [("value", VSimpleType "Int")]
    requiresExpr = []
    ensuresExpr = []
    statements = body
    funcName = getCellName colNo rowNo


encodeexpr :: Expr -> VExpr
encodeexpr expr = case expr of
  EConstInt value -> VIntLit (toInteger value)             -- integer constant
  EBinaryOp subExpr1 op subExpr2 -> VBinaryOp (encodeexpr subExpr1) op (encodeexpr subExpr2) -- binary operation
  EVar variable -> VVar variable      -- global or local variable
  _ -> VNullLit
  -- TODO:
  -- | EParens Expr               -- expression grouped in parentheses
  -- | EUnaryOp String Expr       -- unary operation
  -- | ECell CellPos

  -- EXTRA: | EConstBool Bool            -- true, false
  -- EXTRA: | ECall String [Expr]
  -- EXTRA: | ERange CellPos CellPos


-- HELPERS


getCellName :: Int -> Int -> String
getCellName colNo rowNo = (intToAscii (colNo+65)) ++ show (rowNo + 1)

concatMapWithIndex :: (Int -> a -> [b]) -> [a] -> [b]
concatMapWithIndex f xs = concat $ zipWith f [0..] xs

intToAscii :: Int -> String
intToAscii n = [chr n]
