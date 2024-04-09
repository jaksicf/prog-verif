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

import Spreadsheet.Ast

-- Viper AST types are available as `VProgram`, `VExpr`, etc.
import Viper.Ast

encodeexpr :: Expr -> VExpr
encodeexpr expr = undefined

encodeprogram :: Cell -> VMember
encodeprogram cell = undefined


-- Cells can depend on other cells, so encode them in a graph like way to construct the
-- Viper prog. Like from leaf to root cells, where leaf cells are cells without a
-- dependency.
encode :: Spreadsheet -> VProgram
encode sheet = VProgram [mainMethod] preludeString
  where
    preludeString = ""
    mainMethod = VMethod "mainMethod" argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))
      where
        argsDecl = []
        returnsDecl = []
        requiresExpr = []
        ensuresExpr = []
        statements = [VComment "my amazing comment"]

-- WIP: just encode each cell as it's own program and concat them
-- encode sheet = concatMap concatRow sheet
--   where
--     concatRow row = concatMap encodeprogram row