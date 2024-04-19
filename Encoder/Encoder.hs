-------------
-- ENCODER --
-------------

-- This is the (skeleton of the) spreadsheet to Viper encoder; you will need to
-- implement the `myEncodeexpr`, `encodeprogram`, and `encode` methodtion in this
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
    preludeString = if isSheetCyclic sheet then error "Error: cycle detected" else trace "NOT CYCLIC" ""
    cellMethods = createMethodsForCells sheet []

isSheetCyclic :: Spreadsheet -> Bool
isSheetCyclic [] = False
isSheetCyclic sheet = any (\(Graph.Node rootLabel subForest) -> subForest /= []) (trace (show scc) scc)
  where
    edges = concatMap2DArrayWithIndexPos sheet (createEdge sheet)
    (graph, _, _) = Graph.graphFromEdges (trace (show edges) edges)
    -- no loop => array of nodes where each node's subforrest is []
    -- loop => at least node node's subforrest is not []
    scc = Graph.scc graph


createEdge :: [[Cell]] -> CellPos -> Cell -> [(String, Int, [Int])]
createEdge sheet pos cell = [(getCellNamePos pos, convertCellPosToEdgeIndex pos, outgoing)]
  where
    n_rows = maximum $ map (\row -> length row) sheet
    convertCellPosToEdgeIndex (col, row) = col * n_rows + row
    validCells = getValidCells sheet
    validCellsPos = map fst validCells
    usedCells = getUsedCellsIn validCellsPos cell
    outgoing = map convertCellPosToEdgeIndex usedCells



getUsedCellsIn :: [CellPos] -> Cell -> [CellPos]
getUsedCellsIn validCells CEmpty = []
getUsedCellsIn validCells (CConst _) = []
getUsedCellsIn validCells (CInput _) = []
getUsedCellsIn validCells (CProgram code postcond _) = removeDuplicates ((usedCellsInCode validCells code) ++ (usedCellsInPostcond validCells postcond))

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
  concatMap2DArrayWithIndex sheet (\colIndex rowIndex cell -> createMethodForCell validCells rowIndex colIndex cell otherMethods)

-- func is a function which takes (rowIndex colIndex elem)
concatMap2DArrayWithIndex :: [[a]] -> (Int -> Int -> a -> [b]) -> [b]
concatMap2DArrayWithIndex arr func = concatMapWithIndex concatRow arr
  where
    concatRow rowIndex row = concatMapWithIndex (\colIndex elementOfArray -> func colIndex rowIndex elementOfArray) row

concatMap2DArrayWithIndexPos :: [[a]] -> ((Int, Int) -> a -> [b]) -> [b]
concatMap2DArrayWithIndexPos arr func = concatMap2DArrayWithIndex arr (\colIndex rowIndex elem -> func (colIndex, rowIndex) elem)



createMethodForCell :: [(CellPos, String)] -> Int -> Int -> Cell -> [VMember] -> [VMember]
-- 👻 empty
createMethodForCell validCells rowNo colNo CEmpty otherMethods = []

-- 📝 input
createMethodForCell validCells rowNo colNo (CInput assumedExpr) otherMethods =
  let argsDecl = []
      cellName = getCellName colNo rowNo
      methodName = "f_" ++ cellName
      returnsDecl = [(cellName, VSimpleType "Int")]
      requiresExpr = []
  in case assumedExpr of
    Nothing         -> [VMethod methodName argsDecl returnsDecl requiresExpr []          (Just (VSeq [VComment "📝 input cell"]))]
    Just expression -> [VMethod methodName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
      where
        validCellsPos = map fst validCells
        ensuresExpr = [encodeexprWithRename validCellsPos cellName expression]
        statements = [
          VComment "📝 input cell"
          -- single "assume" in body of method, needed so that ensures of method is fulfilled
          , VAssume (encodeexprWithRename validCellsPos cellName expression)]

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
    validCellsPos = map fst validCells
    cellName = getCellName colNo rowNo
    methodName = "f_" ++ cellName
    usedCells = removeDuplicates ((usedCellsInCode validCellsPos code) ++ (usedCellsInPostcond validCellsPos postcond))
    usedNonTranspCells = getNonTransparentCells validCells usedCells
    usedTranspCells = removeElems usedCells usedNonTranspCells
    requiresExpr = requiresExprFromUsedCells validCellsWithoutSelf otherMethods usedCells
    argsDecl = argsDeclFromUsedCells usedTranspCells otherMethods usedNonTranspCells
    returnsDecl = [(cellName, VSimpleType "Int")]
    ensuresExpr = ensuresExprFromPostcond validCellsWithoutSelf cellName postcond
    statements = [VComment "💻 program cell"] ++ encodeCode validCells cellName code

-- 💻 program, 🧊 transparent
createMethodForCell validCells rowNo colNo (CProgram code (Just _) True) otherMethods = error "Transparent cell can't have post condition"
createMethodForCell validCells rowNo colNo (CProgram code Nothing True) otherMethods = [
  -- macro for transparent cell which holds the cells program
  VMacroStmt ("macro__" ++ methodName) [] (VSeq statements)
  -- Method also created for transparent cells, if we didn't then assertions in transparent
  -- cells wouldn't get checked unless another cell uses this transparent cell (cuz
  -- macros aren't checked unless used).
  -- Method also needs to be created so that the fixpoint calulation can work for other
  -- cells which use a transparent cell, albeit the methods are a bit different compared
  -- to non-transparent cells' methods. That is, we set the "ensures" of transparent cells
  -- be the same as their "requires" because then the constraints can correctly propagate
  -- in the fixpoint calculation through transparent cells' methods. By doing so we
  -- basically make transparent cells pass on their constraints to the other cells which
  -- use them. And we can be sure that having ensures be the same as requires will never
  -- fail the Viper verification as the only variables used in ensures are cells and we
  -- can't change cell's values in a program, so whenever an ensures holds we can be sure
  -- that it will also hold at the end of the program, so the identical requires will also hold.
  , VMethod methodName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VMacroCall ("macro__" ++ methodName) []))
  ]
  where
    validCellsWithoutSelf = filter (\(pos, _) -> pos /= (colNo, rowNo)) validCells
    validCellsPos = map fst validCells
    cellName = getCellName colNo rowNo
    methodName = "f_" ++ cellName
    usedCells = removeDuplicates ((usedCellsInCode validCellsPos code))
    usedNonTranspCells = getNonTransparentCells validCells usedCells
    usedTranspCells = removeElems usedCells usedNonTranspCells
    requiresExpr = requiresExprFromUsedCells validCellsWithoutSelf otherMethods usedCells
    argsDecl = argsDeclFromUsedCells usedTranspCells otherMethods usedNonTranspCells
    returnsDecl = [(cellName, VSimpleType "Int")]
    -- ensures is same as requires, to make the fixpoint calculation work
    ensuresExpr = requiresExpr
    statements = [
      VComment "💻🧊 transparent program cell"
      , VVarDecl cellName (VSimpleType "Int")
      ] ++ encodeCode validCells cellName code

-- Args of a method are the cell variables which are used in a program. Additionally, if
-- any of the used cells is a transparent cell then we also need to get its args.
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
    validCellsPos = map fst validCells
    usedCells = usedCellsInPostcond validCellsPos (Just expr)
    -- only input & const cells can be used in requires
    haveCellsCorrectType = all (\(pos) ->  not $ "PROG" `isPrefixOf` (getCellType validCells pos)) usedCells
    -- haveCellsCorrectType = True
    helper = if haveCellsCorrectType then [encodeexprWithRename validCellsPos cellName expr] else error ("Error: postcond can only use const & input cells" ++ cellName)


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

-- Rename "value" to cell's name (e.g. A1 or B3).
-- Rename other variables so that their names have format "${cellName}__{varName}", this
-- prevents collision between variable names when the same variable is used in a cell and
-- an transparent cell (cuz transparent cells are just macros, so their code get's copy pasted).
-- Prevents collision as no two cells share the same name, so variables can't collide likewise.
encodeexprWithRename :: [CellPos] -> String -> Expr -> VExpr
encodeexprWithRename validCells cellName expr =
  let encodeexprWithRenameHelper = encodeexprWithRename validCells
  in case expr of
    EVar variable -> if variable /= "value" then VVar (cellName ++ "__" ++ variable) else VVar cellName     -- global or local variable
    EUnaryOp op expr -> VUnaryOp op (encodeexprWithRenameHelper cellName expr)      -- unary operation
    EBinaryOp subExpr1 op subExpr2 -> VBinaryOp (encodeexprWithRenameHelper cellName subExpr1) op (encodeexprWithRenameHelper cellName subExpr2) -- binary operation
    EParens expr -> encodeexprWithRenameHelper cellName expr               -- expression grouped in parentheses
    _ -> myEncodeexpr validCells expr

encodeexpr :: Expr -> VExpr
encodeexpr = myEncodeexpr []

myEncodeexpr :: [CellPos] -> Expr -> VExpr
myEncodeexpr validCells expr =
  let encodeHelper = myEncodeexpr validCells in
  case expr of
    EConstInt value -> VIntLit (toInteger value)             -- integer constant
    EBinaryOp subExpr1 op subExpr2 -> VBinaryOp (encodeHelper subExpr1) op (encodeHelper subExpr2) -- binary operation
    EVar variable -> VVar variable      -- global or local variable
    ECell (col, row) -> VVar (getCellName col row)
    EUnaryOp op expr -> VUnaryOp op (encodeHelper expr)      -- unary operation
    EParens expr -> encodeHelper expr               -- expression grouped in parentheses
    EConstBool bool -> if bool then VTrueLit else VFalseLit            -- true, false
    -- EXTRA:
    ECall aggFuncName [] -> error "Error: missing range in agg operation"
    ECall aggFuncName [ERange startPos endPos] -> encodeAggFunc validCells aggFuncName startPos endPos
    ECall aggFuncName _ -> error "Error: invalid expression" -- everything else except range is invalid per interpreter
    ERange startPos endPos -> error "Error: range can only be used inside agg function" -- per interpreter

encodeAggFunc :: [CellPos] -> String -> CellPos -> CellPos -> VExpr
encodeAggFunc validCells aggName startCell endCell = chainedExpression
  where
    op = case aggName of
      "sum" -> "+"
      "product" -> "*"
    usedCells = getUsedCellsFromRange startCell endCell
    usedValidCells = filter (\cell -> cell `elem` validCells) usedCells
    chainedExpression = encodeChainedExpr usedValidCells op

getUsedCellsFromRange :: CellPos -> CellPos -> [CellPos]
getUsedCellsFromRange (starCol, startRow) (endCol, endRow) = [(col, row) | col <- [starCol..endCol], row <- [startRow..endRow]]

encodeChainedExpr :: [CellPos] -> String -> VExpr
encodeChainedExpr [] "+" = VIntLit (toInteger 0)
encodeChainedExpr [] "*" = VIntLit (toInteger 1)
encodeChainedExpr (cell: otherCells) op = VBinaryOp (VVar (getCellNamePos cell)) op (encodeChainedExpr otherCells op)

encodeCode :: [(CellPos, String)] -> String -> [Stmt] -> [VStmt]
encodeCode validCells cellName code = transparentCellsCalls ++ [VComment "HOISTED"] ++ (encodeCodeSub validCellsPos cellName code) ++ [VLabel (cellName ++ "__end")]
  where
    validCellsPos = map fst validCells
    cellVars = findCellVarsInCode validCellsPos code
    transparentCells = filter (\(_, cType) -> cType == "PROG_TRANS")  $ map (\pos -> (pos, getCellType validCells pos)) cellVars
    transparentCellsCalls = map (\(pos, _) -> VMacroCall ("macro__f_" ++ getCellNamePos pos) []) transparentCells
    -- hoistedLocals = genLocalsForCells cellVars

encodeCodeSub :: [CellPos] -> String -> Code -> [VStmt]
encodeCodeSub validCells cellName code =
  map encodeStmt code
  where
    encodeCodeSubHelper = encodeCodeSub validCells
    encodeexprWithRenameHelper = encodeexprWithRename validCells
    encodeStmt stmt = case stmt of
      Skip                -> VComment "Skip stmtm was here"   -- no-op
      Assign varName expr -> VVarAssign (cellName++ "__"  ++ varName) (encodeexprWithRenameHelper cellName expr) -- assignment to variable
      Cond expr ifCode elseCode -> VIf (encodeexprWithRenameHelper cellName expr) (VSeq (encodeCodeSubHelper cellName ifCode)) (VSeq (encodeCodeSubHelper cellName elseCode)) -- conditional; note that `elif` is represented as  another conditional in the `else` branch
      Assert expr         -> VAssert (encodeexprWithRenameHelper cellName expr)   -- assertion
      Local varName varType Nothing     -> VSeq [VVarDecl (cellName ++ "__" ++ varName) (VSimpleType (show varType)), VVarAssign (cellName ++ "__" ++ varName) (if varType == Int then VIntLit 0 else VFalseLit)] -- local variable declaration
      Local varName varType (Just expr) -> VSeq [VVarDecl (cellName ++ "__" ++ varName) (VSimpleType (show varType)), VVarAssign (cellName ++ "__" ++ varName) (encodeexprWithRenameHelper cellName expr)] -- local variable declaration
      Return expr         -> VSeq [VVarAssign cellName (encodeexprWithRenameHelper cellName expr), VGoto (cellName ++ "__end")]
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
    allCells = concatMap2DArrayWithIndex sheet (\colIndex rowIndex cell -> [((colIndex, rowIndex), cellType cell)])
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
getCellType validCells cell =
  let cellTypeWrapper = Map.lookup cell (Map.fromList validCells)
  in case cellTypeWrapper of
    Just cellType -> cellType
    Nothing -> error ("Error: trying to access invalid cell " ++ getCellNamePos cell)


usedCellsInCode :: [CellPos] -> [Stmt] -> [CellPos]
usedCellsInCode = findCellVarsInCode

usedCellsInPostcond :: [CellPos] -> Maybe Expr -> [CellPos]
usedCellsInPostcond validCells Nothing = []
usedCellsInPostcond validCells (Just expr) = findCellVarsInExpr validCells expr

findCellVarsInCode :: [CellPos] -> [Stmt] -> [(Int, Int)]
findCellVarsInCode validCells code = removeDuplicates (concatMap findCellVarsInStmt code)
  where
    findCellVarsInExprHelper = findCellVarsInExpr validCells
    findCellVarsInCodeHelper = findCellVarsInCode validCells
    findCellVarsInStmt stmt = case stmt of
      Skip                -> []
      Assign varName expr -> findCellVarsInExprHelper expr -- assignment to variable
      Cond expr ifCode elseCode -> (findCellVarsInExprHelper expr) ++ (findCellVarsInCodeHelper ifCode) ++ (findCellVarsInCodeHelper elseCode)
      Assert expr         -> findCellVarsInExprHelper expr   -- assertion
      Local varName varType Nothing -> []
      Local varName varType (Just expr) -> findCellVarsInExprHelper expr
      Return expr         -> findCellVarsInExprHelper expr
      Nondet _ _          -> error "non-deterministic choice (not used in this project!)"

findCellVarsInExpr :: [CellPos] -> Expr -> [(Int, Int)]
findCellVarsInExpr validCells expr =
  let findCellVarsInExprHelper = findCellVarsInExpr validCells
  in case expr of
    ECell (col, row) -> [(col, row)]
    EBinaryOp expr1 op expr2 -> (findCellVarsInExprHelper expr1) ++ (findCellVarsInExprHelper expr2) -- binary operation
    EVar variable -> []      -- global or local variable
    EConstInt value -> []             -- integer constant
    EParens expr -> findCellVarsInExprHelper expr      -- expression grouped in parentheses
    EUnaryOp _ expr -> findCellVarsInExprHelper expr      -- unary operation
    ECall _ exprs -> concatMap findCellVarsInExprHelper exprs
    ERange start end -> usedValidCells
      where
        usedCells = getUsedCellsFromRange start end
        -- Only return valid ones, so range can even include comments and blank cells,
        -- but still succeed. Mimics behavior of interpreter
        usedValidCells = filter (\cell -> cell `elem` validCells) usedCells
    _ -> undefined


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
