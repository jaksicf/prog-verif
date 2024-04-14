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
import Data.List (sort, nub, find)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Spreadsheet.Ast

-- Viper AST types are available as `VProgram`, `VExpr`, etc.
import Viper.Ast

encodeprogram :: Cell -> VMember
encodeprogram cell = head (makeCellFunctions [[cell]] [])


encode :: Spreadsheet -> VProgram
encode sheet = VProgram cellFunctions preludeString
  where
    preludeString = ""
    cellFunctions = makeCellFunctions sheet []


-- Fixpoint computation for "requires". We are using the max number of iteration instead
-- of looping until the result doesn't change cuz it's simpler to implement and achieves
-- the same. Basically use a big enough number so that "requires" can propagate along
-- longest path. The longest pathlen is at most n_cells (as we disallow loops), but we
-- are using double that just in case.
makeCellFunctions :: [[Cell]] -> [VMember] -> [VMember]
makeCellFunctions sheet otherMethods = foldr iterateUntilFixpoint [] [0..(length validCells * 2)]
  where
    validCells = getValidCells sheet
    iterateUntilFixpoint _ acc = makeCellFunctionsHelper validCells sheet acc

getValidCells :: [[Cell]] -> [(CellPos, String)] -- (CelPos, CellType) where CellType = {"IN", "PROG", "CONST"}
getValidCells sheet = filter (\(_, cType) -> cType /= "EMPTY") allCells -- remove empty cells
  where
    allCells = concatMapWithIndex concatRow sheet
    concatRow rowIndex row = concatMapWithIndex (\colIndex cell -> [((colIndex, rowIndex), cellType cell)]) row
    cellType cell = case cell of
      CEmpty -> "EMPTY" -- empty cell or comment cell
      CConst _ -> "CONST"
      CInput _ -> "IN"
      CProgram _ _ _ -> "PROG"


makeCellFunctionsHelper :: [(CellPos, String)] -> [[Cell]] -> [VMember] -> [VMember]
makeCellFunctionsHelper validCells sheet otherMethods = concatMapWithIndex concatRow sheet
  where
    concatRow rowIndex row = concatMapWithIndex (\colIndex cell -> makeCellFunc validCells rowIndex colIndex cell otherMethods) row


makeCellFunc :: [(CellPos, String)] -> Int -> Int -> Cell -> [VMember] -> [VMember]
-- 👻 empty
makeCellFunc validCells rowNo colNo CEmpty otherMethods = []

-- 📝 input
makeCellFunc validCells rowNo colNo (CInput assumedExprCell) otherMethods =
  let argsDecl = []
      cellName = getCellName colNo rowNo
      funcName = "f_" ++ cellName
      returnsDecl = [(cellName, VSimpleType "Int")]
      requiresExpr = []
      assumedExprViper = case assumedExprCell of
        Just expression -> [encodeexpr expression]
        Nothing -> []
  in case assumedExprCell of
  Nothing         -> [VMethod funcName argsDecl returnsDecl requiresExpr []          (Just (VSeq statements))]
    where
      statements = [VComment "📝 input cell"]
  Just expression -> [VMethod funcName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
    where
      ensuresExpr = [encodeexprWithRename cellName expression]
      statements = [
        VComment "📝 input cell"
        , VAssume (encodeexprWithRename cellName expression)]

-- #️⃣ const
makeCellFunc validCells rowNo colNo (CConst cellValue) otherMethods = [VMethod funcName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
  where
    cellName = getCellName colNo rowNo
    funcName = "f_" ++ cellName
    argsDecl = []
    returnsDecl = [(cellName, VSimpleType "Int")]
    requiresExpr = []
    ensuresExpr = [VBinaryOp (VVar cellName) "==" (VIntLit (toInteger cellValue))]
    statements = [VComment "#️⃣ const cell",
            VVarAssign cellName (VIntLit (toInteger cellValue))]

-- 💻 program
makeCellFunc validCells rowNo colNo (CProgram code postcond isTransp) otherMethods = [VMethod funcName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
  where
    validCellsWithoutSelf = filter (\(pos, _) -> pos /= (colNo, rowNo)) validCells
    cellName = getCellName colNo rowNo
    funcName = "f_" ++ cellName
    usedCells = removeDuplicates ((usedCellsInCode code) ++ (usedCellsInPostcond postcond))
    requiresExpr = requiresExprFromUsedCells validCellsWithoutSelf otherMethods usedCells
    argsDecl = argsDeclFromUsedCells usedCells
    returnsDecl = [(cellName, VSimpleType "Int")]
    ensuresExpr = ensuresExprFromPostcond validCellsWithoutSelf cellName postcond
    statements = [VComment "💻 program cell"] ++ encodeCode cellName code

argsDeclFromUsedCells :: [CellPos] -> [(String, VType)]
argsDeclFromUsedCells cells = map (\cell -> (getCellNamePos cell,  VSimpleType "Int")) cells


ensuresExprFromPostcond validCells cellName Nothing = []
ensuresExprFromPostcond validCells cellName (Just expr) = helper
  where
    usedCells = usedCellsInPostcond (Just expr)
    -- only input & const cells can be used in requires
    haveCellsCorrectType = all (\(pos) -> (getCellType validCells pos) /= "PROG") usedCells
    helper = if haveCellsCorrectType then [encodeexprWithRename cellName expr] else error "Error: postcond can only use const & input cells"


requiresExprFromUsedCells :: [(CellPos, String)] -> [VMember] -> [CellPos] -> [VExpr]
requiresExprFromUsedCells validCells methods usedCells = helper
  where
    hasInvalidCellBeenUsed = not (containsAll usedCells (map (\(pos, _) -> pos) validCells))
    helper = if hasInvalidCellBeenUsed then error "Error: used invalid cell" else concatMap (requiresExprFromUsedCell methods) usedCells

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll subList mainList = all (`elem` mainList) subList

getCellType :: [(CellPos, String)] -> CellPos -> String
getCellType validCells cell = fromJust (Map.lookup cell (Map.fromList validCells))

requiresExprFromUsedCell :: [VMember] -> CellPos -> [VExpr]
requiresExprFromUsedCell methods cell = ensuresExpr
  where
    cellFuncName =  "f_" ++ (getCellNamePos cell)
    VMethod _ _ _ _ ensuresExpr _  = head (filter (\(VMethod name _ _ _ _ _) -> name == cellFuncName) methods)


encodeexprWithRename :: String -> Expr -> VExpr
encodeexprWithRename newName expr = case expr of
  EVar variable -> if variable /= "value" then VVar variable else VVar newName     -- global or local variable
  EUnaryOp op expr -> VUnaryOp op (encodeexprWithRename newName expr)      -- unary operation
  EBinaryOp subExpr1 op subExpr2 -> VBinaryOp (encodeexprWithRename newName subExpr1) op (encodeexprWithRename newName subExpr2) -- binary operation
  EParens expr -> encodeexprWithRename newName expr               -- expression grouped in parentheses
  _ -> encodeexpr expr
  -- TODO:
  -- EXTRA: | ECall String [Expr]
  -- EXTRA: | ERange CellPos CellPos

encodeexpr :: Expr -> VExpr
encodeexpr expr = case expr of
  EConstInt value -> VIntLit (toInteger value)             -- integer constant
  EBinaryOp subExpr1 op subExpr2 -> VBinaryOp (encodeexpr subExpr1) op (encodeexpr subExpr2) -- binary operation
  EVar variable -> VVar variable      -- global or local variable
  ECell (col, row) -> VVar (getCellName col row)
  EUnaryOp op expr -> VUnaryOp op (encodeexpr expr)      -- unary operation
  EParens expr -> encodeexpr expr               -- expression grouped in parentheses
  -- EXTRA:
  EConstBool bool -> if bool then VTrueLit else VFalseLit            -- true, false
  _ -> error "NOT IMPLEMENTED YET"
  -- TODO:
  -- EXTRA: | ECall String [Expr]
  -- EXTRA: | ERange CellPos CellPos

encodeCode :: String -> [Stmt] -> [VStmt]
encodeCode cellName code = (encodeCodeSub cellName code) ++ [VLabel (cellName ++ "__end")]
  where
    cellVars = findCellVarsInCode code
    hoistedLocals = genLocalsForCells cellVars

encodeCodeSub :: String -> Code -> [VStmt]
encodeCodeSub cellName code = map encodeStmt code
  where
    encodeStmt stmt = case stmt of
      Skip                -> VComment "Skip stmtm was here"   -- no-op
      Assign varName expr -> VVarAssign varName (encodeexpr expr) -- assignment to variable
      Cond expr ifCode elseCode -> VIf (encodeexpr expr) (VSeq (encodeCodeSub cellName ifCode)) (VSeq (encodeCodeSub cellName elseCode)) -- conditional; note that `elif` is represented as  another conditional in the `else` branch
      Assert expr         -> VAssert (encodeexpr expr)   -- assertion
      Local varName varType Nothing     -> VSeq [VVarDecl varName (VSimpleType (show varType)), VVarAssign varName (if varType == Int then VIntLit 0 else VFalseLit)] -- local variable declaration
      Local varName varType (Just expr) -> VSeq [VVarDecl varName (VSimpleType (show varType)), VVarAssign varName (encodeexpr expr)] -- local variable declaration
      Return expr         -> VSeq [VVarAssign cellName (encodeexpr expr), VGoto (cellName ++ "__end")]
      Nondet _ _          -> error "non-deterministic choice (not used in this project!)"


-- HELPERS
genLocalsForCells :: [(Int, Int)] -> [VStmt]
genLocalsForCells cells = map genLocalForCel cells
  where
    genLocalForCel (col, row) = VSeq [VVarDecl varName (VSimpleType "Int"), VVarAssign varName (VFuncApp (getCellName col row) [])]
      where
      varName = "__" ++ (getCellName col row)

usedCellsInCode :: [Stmt] -> [CellPos]
usedCellsInCode = findCellVarsInCode

usedCellsInPostcond :: Maybe Expr -> [CellPos]
usedCellsInPostcond Nothing = []
usedCellsInPostcond (Just expr) = findCellVarsInExpr expr

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

findCellVarsInExpr :: Expr -> [(Int, Int)]
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

getCellNamePos :: CellPos -> String
getCellNamePos (colNo, rowNo) = getCellName colNo rowNo

concatMapWithIndex :: (Int -> a -> [b]) -> [a] -> [b]
concatMapWithIndex f xs = concat $ zipWith f [0..] xs

intToAscii :: Int -> String
intToAscii n = [chr n]
