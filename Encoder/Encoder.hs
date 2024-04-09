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

encode :: Spreadsheet -> VProgram
encode sheet = undefined
