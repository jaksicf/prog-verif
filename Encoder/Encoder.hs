-------------
-- ENCODER --
-------------

-- This is the (skeleton of the) spreadsheet to Viper encoder; you will need to
-- implement the `encodeexpr`, `encodeprogram`, and `encode` methodtion in this
-- module.

module Encoder.Encoder (
  encodeexpr,
  encodeprogram,
  encode,
) where

import Data.Char (chr)
import Data.List (sort, nub, find)
import Data.Maybe (fromJust)
import Data.List (isPrefixOf)
import qualified Data.Map as Map

import qualified Data.Graph as Graph

import Spreadsheet.Ast

-- Viper AST types are available as `VProgram`, `VExpr`, etc.
import Viper.Ast
import Debug.Trace (trace)

encodeprogram :: Cell -> VMember
encodeprogram cell = head (createMethodsForCells [[cell]] [])


encode :: Spreadsheet -> VProgram
encode sheet = VProgram cellMethods preludeString
  where
    preludeString = ""
    cellMethods = createMethodsForCells sheet []


-- Fixpoint computation for "requires". We are using the max number of iteration instead
-- of looping until the result doesn't change cuz it's simpler to implement and achieves
-- the same. Basically use a big enough number so that "requires" can propagate along
-- longest path. The longest pathlen is at most n_cells (as we disallow loops), but we
-- are using double that just in case.
createMethodsForCells :: [[Cell]] -> [VMember] -> [VMember]
createMethodsForCells sheet otherMethods = foldr iterateUntilFixpoint [] [0..(length validCells * 2)]
  where
    validCells = getValidCells sheet
    iterateUntilFixpoint _ acc = createMethodsForCellsHelper validCells sheet acc

createMethodsForCellsHelper :: [(CellPos, String)] -> [[Cell]] -> [VMember] -> [VMember]
createMethodsForCellsHelper validCells sheet otherMethods =
  concatMap2DArrayWithIndex sheet (\rowIndex colIndex cell -> createMethodForCell validCells rowIndex colIndex cell otherMethods)

-- func is a function which takes (rowIndex colIndex elem)
concatMap2DArrayWithIndex :: [[a]] -> (Int -> Int -> a -> [b]) -> [b]
concatMap2DArrayWithIndex arr func = concatMapWithIndex concatRow arr
  where
    concatRow rowIndex row = concatMapWithIndex (\colIndex elementOfArray -> func rowIndex colIndex elementOfArray) row


createMethodForCell :: [(CellPos, String)] -> Int -> Int -> Cell -> [VMember] -> [VMember]
-- 👻 empty
createMethodForCell validCells rowNo colNo CEmpty otherMethods = []

-- 📝 input
createMethodForCell validCells rowNo colNo (CInput assumedExprCell) otherMethods =
  let argsDecl = []
      cellName = getCellName colNo rowNo
      methodName = "f_" ++ cellName
      returnsDecl = [(cellName, VSimpleType "Int")]
      requiresExpr = []
      assumedExprViper = case assumedExprCell of
        Just expression -> [encodeexpr expression]
        Nothing -> []
  in case assumedExprCell of
    Nothing         -> [VMethod methodName argsDecl returnsDecl requiresExpr []          (Just (VSeq [VComment "📝 input cell"]))]
    Just expression -> [VMethod methodName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
      where
        ensuresExpr = [encodeexprWithRename cellName expression]
        statements = [
          VComment "📝 input cell"
          , VAssume (encodeexprWithRename cellName expression)]

-- #️⃣ const
createMethodForCell validCells rowNo colNo (CConst cellValue) otherMethods = [VMethod methodName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
  where
    cellName = getCellName colNo rowNo
    methodName = "f_" ++ cellName
    argsDecl = []
    returnsDecl = [(cellName, VSimpleType "Int")]
    requiresExpr = []
    ensuresExpr = [VBinaryOp (VVar cellName) "==" (VIntLit (toInteger cellValue))]
    statements = [VComment "#️⃣ const cell",
            VVarAssign cellName (VIntLit (toInteger cellValue))]

-- 💻 program, 🧱 NOT transparent (default)
createMethodForCell validCells rowNo colNo (CProgram code postcond False) otherMethods = [VMethod methodName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
  where
    validCellsWithoutSelf = filter (\(pos, _) -> pos /= (colNo, rowNo)) validCells
    cellName = getCellName colNo rowNo
    methodName = "f_" ++ cellName
    usedCells = removeDuplicates ((usedCellsInCode code) ++ (usedCellsInPostcond postcond))
    usedNonTranspCells = getNonTransparentCells validCells usedCells
    usedTranspCells = removeElems usedCells usedNonTranspCells
    requiresExpr = requiresExprFromUsedCells validCellsWithoutSelf otherMethods usedCells
    argsDecl = argsDeclFromUsedCells usedTranspCells otherMethods usedNonTranspCells
    returnsDecl = [(cellName, VSimpleType "Int")]
    ensuresExpr = ensuresExprFromPostcond validCellsWithoutSelf cellName postcond
    statements = [VComment "💻 program cell"] ++ encodeCode validCells cellName code

-- 💻 program, 🧊 transparent
createMethodForCell validCells rowNo colNo (CProgram code (Just _) True) otherMethods = error "Transparent cell can't have postcondition"
createMethodForCell validCells rowNo colNo (CProgram code Nothing True) otherMethods = [
  VMacroStmt ("macro__" ++ methodName) [] (VSeq statements)
  , VMethod methodName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VMacroCall ("macro__" ++ methodName) []))
  ]
  where
    validCellsWithoutSelf = filter (\(pos, _) -> pos /= (colNo, rowNo)) validCells
    cellName = getCellName colNo rowNo
    methodName = "f_" ++ cellName
    usedCells = removeDuplicates ((usedCellsInCode code))
    usedNonTranspCells = getNonTransparentCells validCells usedCells
    usedTranspCells = removeElems usedCells usedNonTranspCells
    requiresExpr = requiresExprFromUsedCells validCellsWithoutSelf otherMethods usedCells
    argsDecl = argsDeclFromUsedCells usedTranspCells otherMethods usedNonTranspCells
    returnsDecl = [(cellName, VSimpleType "Int")]
    ensuresExpr = requiresExpr
    statements = [
      VComment "💻🧊 transparent program cell"
      , VVarDecl cellName (VSimpleType "Int")
      ] ++ encodeCode validCells cellName code

argsDeclFromUsedCells :: [CellPos] -> [VMember] -> [CellPos] -> [(String, VType)]
argsDeclFromUsedCells transpCells otherMethods cells = argsFromLocal ++ argsFromTransparentMethods
  where
    argsFromLocal = map (\cell -> (getCellNamePos cell,  VSimpleType "Int")) cells
    argsFromTransparentMethods = concatMap getArgsFromTransparentMethods transpCells
    getArgsFromTransparentMethods cell = argsDecl
      where
        cellMethodName =  "f_" ++ (getCellNamePos cell)
        getMethodName (VMethod name _ _ _ _ _) = name == cellMethodName
        getMethodName _ = False
        VMethod _ argsDecl _ _ _ _  = head (filter getMethodName otherMethods)


ensuresExprFromPostcond :: [(CellPos, String)] -> [Char] -> Maybe Expr -> [VExpr]
ensuresExprFromPostcond validCells cellName Nothing = []
ensuresExprFromPostcond validCells cellName (Just expr) = helper
  where
    usedCells = usedCellsInPostcond (Just expr)
    -- only input & const cells can be used in requires
    haveCellsCorrectType = all (\(pos) ->  not $ "PROG" `isPrefixOf` (getCellType validCells pos)) usedCells
    -- haveCellsCorrectType = True
    helper = if haveCellsCorrectType then [encodeexprWithRename cellName expr] else error ("Error: postcond can only use const & input cells" ++ cellName)


requiresExprFromUsedCells :: [(CellPos, String)] -> [VMember] -> [CellPos] -> [VExpr]
requiresExprFromUsedCells validCells methods usedCells = helper
  where
    hasInvalidCellBeenUsed = not (containsAll usedCells (map (\(pos, _) -> pos) validCells))
    helper = if hasInvalidCellBeenUsed then error "Error: used invalid cell" else concatMap (requiresExprFromUsedCell methods) usedCells

requiresExprFromUsedCell :: [VMember] -> CellPos -> [VExpr]
requiresExprFromUsedCell methods cell = ensuresExpr
  where
    cellMethodName =  "f_" ++ (getCellNamePos cell)
    getMethodName (VMethod name _ _ _ _ _) = name == cellMethodName
    getMethodName _ = False
    VMethod _ _ _ _ ensuresExpr _  = head (filter getMethodName methods)


encodeexprWithRename :: String -> Expr -> VExpr
encodeexprWithRename cellName expr = case expr of
  EVar variable -> if variable /= "value" then VVar (cellName ++ "__" ++ variable) else VVar cellName     -- global or local variable
  EUnaryOp op expr -> VUnaryOp op (encodeexprWithRename cellName expr)      -- unary operation
  EBinaryOp subExpr1 op subExpr2 -> VBinaryOp (encodeexprWithRename cellName subExpr1) op (encodeexprWithRename cellName subExpr2) -- binary operation
  EParens expr -> encodeexprWithRename cellName expr               -- expression grouped in parentheses
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

encodeCode :: [(CellPos, String)] -> String -> [Stmt] -> [VStmt]
encodeCode validCells cellName code = transparentCellsCalls ++ [VComment "HOISTED"] ++ (encodeCodeSub cellName code) ++ [VLabel (cellName ++ "__end")]
  where
    cellVars = findCellVarsInCode code
    transparentCells = filter (\(_, cType) -> cType == "PROG_TRANS")  $ map (\pos -> (pos, getCellType validCells pos)) cellVars
    transparentCellsCalls = map (\(pos, _) -> VMacroCall ("macro__f_" ++ getCellNamePos pos) []) transparentCells
    -- hoistedLocals = genLocalsForCells cellVars

encodeCodeSub :: String -> Code -> [VStmt]
encodeCodeSub cellName code = map encodeStmt code
  where
    encodeStmt stmt = case stmt of
      Skip                -> VComment "Skip stmtm was here"   -- no-op
      Assign varName expr -> VVarAssign (cellName++ "__"  ++ varName) (encodeexprWithRename cellName expr) -- assignment to variable
      Cond expr ifCode elseCode -> VIf (encodeexprWithRename cellName expr) (VSeq (encodeCodeSub cellName ifCode)) (VSeq (encodeCodeSub cellName elseCode)) -- conditional; note that `elif` is represented as  another conditional in the `else` branch
      Assert expr         -> VAssert (encodeexprWithRename cellName expr)   -- assertion
      Local varName varType Nothing     -> VSeq [VVarDecl (cellName ++ "__" ++ varName) (VSimpleType (show varType)), VVarAssign (cellName ++ "__" ++ varName) (if varType == Int then VIntLit 0 else VFalseLit)] -- local variable declaration
      Local varName varType (Just expr) -> VSeq [VVarDecl (cellName ++ "__" ++ varName) (VSimpleType (show varType)), VVarAssign (cellName ++ "__" ++ varName) (encodeexprWithRename cellName expr)] -- local variable declaration
      Return expr         -> VSeq [VVarAssign cellName (encodeexprWithRename cellName expr), VGoto (cellName ++ "__end")]
      Nondet _ _          -> error "non-deterministic choice (not used in this project!)"


-- HELPERS

-- genLocalsForCells :: [(Int, Int)] -> [VStmt]
-- genLocalsForCells cells = map genLocalForCel cells
--   where
--     genLocalForCel (col, row) = VSeq [VVarDecl varName (VSimpleType "Int"), VVarAssign varName (VMethodApp (getCellName col row) [])]
--       where
--       varName = "__" ++ (getCellName col row)


getValidCells :: [[Cell]] -> [(CellPos, String)] -- (CelPos, CellType) where CellType = {"IN", "CONST", "PROG_NON_TRANS", "PROG_TRANS"}
getValidCells sheet = filter (\(_, cType) -> cType /= "EMPTY") allCells -- remove empty cells
  where
    allCells = concatMap2DArrayWithIndex sheet (\rowIndex colIndex cell -> [((colIndex, rowIndex), cellType cell)])
    cellType cell = case cell of
      CEmpty -> "EMPTY" -- empty cell or comment cell
      CConst _ -> "CONST"
      CInput _ -> "IN"
      CProgram _ _ False -> "PROG_NON_TRANS"
      CProgram _ _ True -> "PROG_TRANS"

getNonTransparentCells :: [(CellPos, String)] -> [CellPos] -> [CellPos]
getNonTransparentCells validCells cells = map (\(pos, _) -> pos) $ filter (\(_, cType) -> cType /= "PROG_TRANS")  $ map (\pos -> (pos, getCellType validCells pos)) cells

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll subList mainList = all (`elem` mainList) subList

getCellType :: [(CellPos, String)] -> CellPos -> String
getCellType validCells cell = fromJust (Map.lookup cell (Map.fromList validCells))

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

removeElems :: Eq a => [a] -> [a] -> [a]
removeElems xs ys = [x | x <- xs, x `notElem` ys]

getCellName :: Int -> Int -> String
getCellName colNo rowNo = (intToAscii (colNo+65)) ++ show (rowNo + 1)

getCellNamePos :: CellPos -> String
getCellNamePos (colNo, rowNo) = getCellName colNo rowNo

concatMapWithIndex :: (Int -> a -> [b]) -> [a] -> [b]
concatMapWithIndex f xs = concat $ zipWith f [0..] xs

intToAscii :: Int -> String
intToAscii n = [chr n]
