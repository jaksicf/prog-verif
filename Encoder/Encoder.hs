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
    sheetWithGeneratedCells = expandSheet sheet
    preludeString = if isSheetCyclic sheetWithGeneratedCells then error "Error: cycle detected" else trace "NOT CYCLIC" ""
    cellMethods = createMethodsForCells sheetWithGeneratedCells []

expandSheet :: Spreadsheet -> Spreadsheet
-- expand the CRow cells into generated cells (part of the "iterated cell ranges" extension)
expandSheet sheet = mapWithIndex expandRow sheet

expandRow :: Int -> [Cell] -> [Cell]
-- While expanding the rows we need to keep track of the row index (after we generate
-- some cells) to handle the case when we first have some other cells (const, comment,
-- prog, etc.) followed by an CRow cell which needs to be expanded. Because then the CRow
-- doesn't start at index 0, but some other index. Same when we have a row which is a mix
-- of diff cell types which include CRows.
expandRow rowIndex row = fst (foldl expansionWhileTrackingColIndex ([], 0) row)
  where
    -- increase the rowIndex by the amount of generated cells
    expansionWhileTrackingColIndex (prevCells, colIndex) cell = (prevCells ++ expanded, colIndex + (length expanded))
      where
        cellPos = (colIndex, rowIndex)
        expanded = expandCell cellPos cell

expandCell :: CellPos -> Cell -> [Cell]
expandCell cellPos cell = case cell of
      CRow len init op -> createGeneratedCells cellPos len init op
      _ -> [cell] -- normal cells are just returned as is

createGeneratedCells :: CellPos -> Int -> Expr -> Expr -> [Cell]
createGeneratedCells cellPos len init op = dependentCells
  where
    (colIndex, rowIndex) = cellPos
    dependentCells = map (\offset -> createGeneratedCell (colIndex + offset, rowIndex) op init offset) [0..len]

-- The 1st generated cell has the constraint "cell $NAME == init".
-- The 2nd generated cell has the constraint "cell $NAME == op(init)".
-- The 3nd generated cell has the constraint "cell $NAME == op(op(init))".
-- And so forth. As such, each of them acts the same way as an (a bit more elaborate) CConst cell.
createGeneratedCell :: CellPos -> Expr -> Expr -> Int -> Cell
createGeneratedCell cellPos opExpr init offset = CGeneerated (equalExpr)
  where
    chainedExpr = foldl (\acc elem -> replaceXWithExpr acc opExpr) init [1..offset]
    equalExpr = EBinaryOp (ECell cellPos) "==" chainedExpr

-- replace the variable "x" with the expression "replacementExpr" in the "opExpr" expression
replaceXWithExpr :: Expr -> Expr -> Expr
replaceXWithExpr replacementExpr opExpr =
  let replacementExprHelper = replaceXWithExpr replacementExpr
  in case opExpr of
    EVar "x" -> replacementExpr     -- global or local variable
    EUnaryOp op expr -> EUnaryOp op (replacementExprHelper expr)      -- unary operation
    EBinaryOp subExpr1 op subExpr2 -> EBinaryOp (replacementExprHelper subExpr1) op (replacementExprHelper subExpr2) -- binary operation
    EParens expr -> EParens (replacementExprHelper expr)               -- expression grouped in parentheses
    _ -> opExpr

-- WELL FORMEDNESS CHECK for cycles
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

-- get a list of cells which have been referenced/used by a given cell
getUsedCellsIn :: [CellPos] -> Cell -> [CellPos]
getUsedCellsIn validCells CEmpty = []
getUsedCellsIn validCells (CConst _) = []
getUsedCellsIn validCells (CInput _) = []
getUsedCellsIn validCells (CProgram code postcond _) = removeDuplicates ((usedCellsInCode validCells code) ++ (usedCellsInPostcond validCells postcond))
getUsedCellsIn validCells (CGeneerated _) = []
getUsedCellsIn validCells (CRow _ _ _) = error "Error: row() should have been replaced by generated cells"

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



createMethodForCell :: [(CellPos, String)] -> Int -> Int -> Cell -> [VMember] -> [VMember]
-- 👻 empty cell
createMethodForCell validCells rowNo colNo CEmpty otherMethods = []

-- 📝 input cell
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

-- #️⃣ const cell
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

-- 🤖 generated cell
createMethodForCell validCells rowNo colNo (CGeneerated assumedExpr) otherMethods = [VMethod methodName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
  where
    validCellsPos = map fst validCells
    usedCells = removeDuplicates (findCellsInExpr validCellsPos assumedExpr)
    usedCellssWithoutSelf = removeElems usedCells [(colNo, rowNo)]
    argsDecl = argsDeclFromUsedCells [] otherMethods usedCellssWithoutSelf
    cellName = getCellName colNo rowNo
    methodName = "f_" ++ cellName
    returnsDecl = [(cellName, VSimpleType "Int")]
    -- generated cells don't require anything as they don't depend on any other cell, same as constant cells
    requiresExpr = []
    ensuresExpr = [encodeexprWithRename validCellsPos cellName assumedExpr]
    statements = [
      VComment "🤖 generated cell"
      -- single "assume" in body of method, needed so that ensures of method is fulfilled
      , VAssume (encodeexprWithRename validCellsPos cellName assumedExpr)]

-- 💻 program, 🧱 NOT transparent (default) cell
createMethodForCell validCells rowNo colNo (CProgram code postcond False) otherMethods = [VMethod methodName argsDecl returnsDecl requiresExpr ensuresExpr (Just (VSeq statements))]
  where
    validCellsPos = map fst validCells
    cellName = getCellName colNo rowNo
    methodName = "f_" ++ cellName
    usedCells = removeDuplicates ((usedCellsInCode validCellsPos code) ++ (usedCellsInPostcond validCellsPos postcond))
    usedNonTranspCells = getNonTransparentCells validCells usedCells
    usedTranspCells = removeElems usedCells usedNonTranspCells
    validCellsWithoutSelf = filter (\(pos, _) -> pos /= (colNo, rowNo)) validCells
    -- "requires" should not reference the cell for which we are generating the method
    -- because an method's output can not be in the "require" clause (duh), so use
    -- validCellsWithoutSelf instead of usedCells. Then if we use the cell itself, we're
    -- going to get an well-formedness error.
    requiresExpr = requiresExprFromUsedCells validCellsWithoutSelf otherMethods usedCells
    argsDecl = argsDeclFromUsedCells usedTranspCells otherMethods usedNonTranspCells
    returnsDecl = [(cellName, VSimpleType "Int")]
    -- postcondition should not reference the the cell for which we are generating the
    -- method (it should use "value" instead), so use validCellsWithoutSelf instead of
    -- usedCells.  Then if we use the cell itself, we're going to get an well-formedness error.
    ensuresExpr = ensuresExprFromPostcond validCellsWithoutSelf cellName postcond
    statements = [VComment "💻 program cell"] ++ encodeCode validCells cellName code

-- 💻 program, 🧊 transparent cell
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
    validCellsPos = map fst validCells
    cellName = getCellName colNo rowNo
    methodName = "f_" ++ cellName
    usedCells = removeDuplicates ((usedCellsInCode validCellsPos code))
    usedNonTranspCells = getNonTransparentCells validCells usedCells
    usedTranspCells = removeElems usedCells usedNonTranspCells
    validCellsWithoutSelf = filter (\(pos, _) -> pos /= (colNo, rowNo)) validCells
    -- "requires" can't reference the cell for which we are generating the method
    -- because an method's output can not be in the "require" clause (duh), so use
    -- validCellsWithoutSelf instead of usedCells. Then if we use the cell itself, we're going to get an well-formedness error.
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

-- computes "ensures" constraints by looking at the postcondition
ensuresExprFromPostcond :: [(CellPos, String)] -> [Char] -> Maybe Expr -> [VExpr]
ensuresExprFromPostcond validCells cellName Nothing = []
ensuresExprFromPostcond validCells cellName (Just expr) = helper
  where
    validCellsPos = map fst validCells
    usedCells = usedCellsInPostcond validCellsPos (Just expr)
    -- only input & const cells can be used in requires
    haveCellsCorrectType = all (\(pos) ->  not $ "PROG" `isPrefixOf` (getCellType validCells pos)) usedCells
    helper = if haveCellsCorrectType then [encodeexprWithRename validCellsPos cellName expr] else error ("Error: postcond can only use const & input cells" ++ cellName)

-- computes "ensures" constraints by looking at which cells the program used/referenced
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

-- here we pass in [] as valid cells
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
    -- EXTRA: basic & advanced aggregation operations
    -- FURTHER EXTENSION: ranges can also go over comments (get ignored) & empty cells, just like in real excel
    ECall aggFuncName [] -> error "Error: missing range in agg operation"
    ECall aggFuncName [ERange startPos endPos] -> encodeAggFunc validCells aggFuncName startPos endPos
    ECall aggFuncName [ERange startPos endPos, initialVal, operation] -> encodeCustomAggFunc validCells initialVal operation startPos endPos
    ECall aggFuncName _ -> error "Error: invalid expression" -- everything else except range is invalid per interpreter
    ERange startPos endPos -> error "Error: range can only be used inside agg function" -- per interpreter

-- EXTRA: advanced aggregation operations
encodeCustomAggFunc :: [CellPos] -> Expr -> Expr -> CellPos -> CellPos -> VExpr
encodeCustomAggFunc validCells initialVal op startCell endCell = chainedExpression
  where
    usedCells = getUsedCellsFromRange startCell endCell
    usedValidCells = filter (\cell -> cell `elem` validCells) usedCells
    encodedInit = myEncodeexpr validCells initialVal
    -- final (chained) expression which you get after applying the function to all elems of the range
    chainedExpression = encodeChainedCustomExpr validCells encodedInit usedValidCells op

encodeChainedCustomExpr :: [CellPos] -> VExpr -> [CellPos] -> Expr -> VExpr
encodeChainedCustomExpr validCells initialVal [] op = initialVal -- base case
encodeChainedCustomExpr validCells initialVal (cell: otherCells) op = -- inductive case
  encodeChainedCustomExpr validCells newInit otherCells op
    where
       newInit = encodeOperationWithRename validCells initialVal ((VVar (getCellNamePos cell))) op

-- Takes the expression which contains x,y (named "op") and returns another expression in which x,y
-- have been replaced with the actual expression for x and y.
encodeOperationWithRename :: [CellPos] -> VExpr -> VExpr -> Expr -> VExpr
encodeOperationWithRename validCells x y op =
  let encodeOperationWithRenameHelper = encodeOperationWithRename validCells x y in
  case op of
    EConstInt value -> VIntLit (toInteger value)             -- integer constant
    EBinaryOp subExpr1 op subExpr2 -> VBinaryOp (encodeOperationWithRenameHelper subExpr1) op (encodeOperationWithRenameHelper subExpr2) -- binary operation
    EVar "x" -> x      -- accumulator
    EVar "y" -> y      -- cell
    EVar _ -> error "Error: in custom aggs only x & y are allowed"
    ECell (col, row) -> VVar (getCellName col row)
    EUnaryOp op expr -> VUnaryOp op (encodeOperationWithRenameHelper expr)      -- unary operation
    EParens expr -> encodeOperationWithRenameHelper expr               -- expression grouped in parentheses
    EConstBool bool -> if bool then VTrueLit else VFalseLit            -- true, false
    -- ranges
    ECall aggFuncName [] -> error "Error: missing range in agg operation"
    ECall aggFuncName [ERange startPos endPos] -> encodeAggFunc validCells aggFuncName startPos endPos
    ECall aggFuncName [ERange startPos endPos, initialVal, operation] -> encodeCustomAggFunc validCells initialVal operation startPos endPos
    ECall aggFuncName _ -> error "Error: invalid expression" -- everything else except range is invalid per interpreter
    ERange startPos endPos -> error "Error: range can only be used inside agg function" -- per interpreter

-- EXTRA: basic aggregation operations
encodeAggFunc :: [CellPos] -> String -> CellPos -> CellPos -> VExpr
encodeAggFunc validCells aggName startCell endCell = chainedExpression
  where
    op = case aggName of
      "sum" -> "+"
      "product" -> "*"
    usedCells = getUsedCellsFromRange startCell endCell
    usedValidCells = filter (\cell -> cell `elem` validCells) usedCells
    chainedExpression = encodeChainedExpr usedValidCells op



encodeChainedExpr :: [CellPos] -> String -> VExpr
encodeChainedExpr [] "+" = VIntLit (toInteger 0)
encodeChainedExpr [] "*" = VIntLit (toInteger 1)
encodeChainedExpr (cell: otherCells) op = VBinaryOp (VVar (getCellNamePos cell)) op (encodeChainedExpr otherCells op)

encodeCode :: [(CellPos, String)] -> String -> [Stmt] -> [VStmt]
encodeCode validCells cellName code = transparentCellsCalls ++ [VComment "HOISTED"] ++ (encodeCodeSub validCellsPos cellName code) ++ [VLabel (cellName ++ "__end")]
  where
    validCellsPos = map fst validCells
    cellVars = findCellsInCode validCellsPos code
    transparentCells = filter (\(_, cType) -> cType == "PROG_TRANS")  $ map (\pos -> (pos, getCellType validCells pos)) cellVars
    transparentCellsCalls = map (\(pos, _) -> VMacroCall ("macro__f_" ++ getCellNamePos pos) []) transparentCells

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

-- Returns all cells in the sheet which aren't EMPTY or COMMENTS. Used later for the
-- WELL FORMDNESS check, that is, before we convert an cell into Viper syntax we check
-- that the cell is actually valid (e.g. exists and is not EMPTY or a COMMENT) and if not, throw an error.
getValidCells :: [[Cell]] -> [(CellPos, String)] -- (CelPos, CellType)
getValidCells sheet = filter (\(_, cType) -> cType /= "EMPTY") allCells -- remove empty cells
  where
    allCells = concatMap2DArrayWithIndex sheet (\colIndex rowIndex cell -> [((colIndex, rowIndex), cellType cell)])
    cellType cell = case cell of
      CEmpty -> "EMPTY" -- empty cell or comment cell
      CConst _ -> "CONST"
      CInput _ -> "IN"
      CProgram _ _ False -> "PROG_NON_TRANS"
      CProgram _ _ True -> "PROG_TRANS"
      CGeneerated _ -> "CONST" -- const, so that it can be used by program cells in their postcondition
      CRow _ _ _ -> error "Error: row() cell should not be instantiated, but rather 'converted' into normal cells"

getCellType :: [(CellPos, String)] -> CellPos -> String
getCellType validCells cell =
  let cellTypeWrapper = Map.lookup cell (Map.fromList validCells)
  in case cellTypeWrapper of
    Just cellType -> cellType
    Nothing -> error ("Error: trying to access invalid cell " ++ getCellNamePos cell)

-- returns list of cells which have been used in a code part
usedCellsInCode :: [CellPos] -> [Stmt] -> [CellPos]
usedCellsInCode = findCellsInCode

-- similar to above but for postcond
usedCellsInPostcond :: [CellPos] -> Maybe Expr -> [CellPos]
usedCellsInPostcond validCells Nothing = []
usedCellsInPostcond validCells (Just expr) = findCellsInExpr validCells expr

-- similar to above but for program code
findCellsInCode :: [CellPos] -> [Stmt] -> [(Int, Int)]
findCellsInCode validCells code = removeDuplicates (concatMap findCellsInStmt code)
  where
    findCellsInExprHelper = findCellsInExpr validCells
    findCellsInCodeHelper = findCellsInCode validCells
    findCellsInStmt stmt = case stmt of
      Skip                -> []
      Assign varName expr -> findCellsInExprHelper expr -- assignment to variable
      Cond expr ifCode elseCode -> (findCellsInExprHelper expr) ++ (findCellsInCodeHelper ifCode) ++ (findCellsInCodeHelper elseCode)
      Assert expr         -> findCellsInExprHelper expr   -- assertion
      Local varName varType Nothing -> []
      Local varName varType (Just expr) -> findCellsInExprHelper expr
      Return expr         -> findCellsInExprHelper expr
      Nondet _ _          -> error "non-deterministic choice (not used in this project!)"

-- similar to above but for expressions
findCellsInExpr :: [CellPos] -> Expr -> [(Int, Int)]
findCellsInExpr validCells expr =
  let findCellsInExprHelper = findCellsInExpr validCells
  in case expr of
    ECell (col, row) -> [(col, row)]
    EBinaryOp expr1 op expr2 -> (findCellsInExprHelper expr1) ++ (findCellsInExprHelper expr2) -- binary operation
    EVar variable -> []      -- global or local variable
    EConstInt value -> []             -- integer constant
    EConstBool value -> []             -- bool constant
    EParens expr -> findCellsInExprHelper expr      -- expression grouped in parentheses
    EUnaryOp _ expr -> findCellsInExprHelper expr      -- unary operation
    ECall _ exprs -> concatMap findCellsInExprHelper exprs
    ERange start end -> usedValidCells
      where
        usedCells = getUsedCellsFromRange start end
        -- Only return valid ones, so range can even include comments and blank cells,
        -- but still succeed. Mimics behavior of interpreter
        usedValidCells = filter (\cell -> cell `elem` validCells) usedCells

-- similar to above but for ranges
-- FURTHER EXTENSION: ranges can also go over comments (get ignored) & empty cells, just like in real excel
getUsedCellsFromRange :: CellPos -> CellPos -> [CellPos]
getUsedCellsFromRange (starCol, startRow) (endCol, endRow) = [(col, row) | col <- [starCol..endCol], row <- [startRow..endRow]]

getNonTransparentCells :: [(CellPos, String)] -> [CellPos] -> [CellPos]
getNonTransparentCells validCells cells = map (\(pos, _) -> pos) $ filter (\(_, cType) -> cType /= "PROG_TRANS")  $ map (\pos -> (pos, getCellType validCells pos)) cells


getCellNamePos :: CellPos -> String
getCellNamePos (colNo, rowNo) = getCellName colNo rowNo

getCellName :: Int -> Int -> String
getCellName colNo rowNo = getColName colNo ++ show (rowNo + 1)

-- if more than 25 cols used, then we do excel style A1->...->Z1->AA1->AB1->...->AZ1->AAA1
getColName :: Int -> [Char]
getColName colNo
  | colNo <= 25 = (intToAscii (colNo+65))
  | otherwise = (getColName ((colNo `div` 26) -1)) ++ (getColName (colNo `mod` 26))


-- GENERAL HELPERS

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = nub . sort

removeElems :: Eq a => [a] -> [a] -> [a]
removeElems xs ys = [x | x <- xs, x `notElem` ys]

-- func is a function which takes (rowIndex colIndex elem)
concatMap2DArrayWithIndex :: [[a]] -> (Int -> Int -> a -> [b]) -> [b]
concatMap2DArrayWithIndex arr func = concatMapWithIndex concatRow arr
  where
    concatRow rowIndex row = concatMapWithIndex (\colIndex elementOfArray -> func colIndex rowIndex elementOfArray) row

concatMap2DArrayWithIndexPos :: [[a]] -> ((Int, Int) -> a -> [b]) -> [b]
concatMap2DArrayWithIndexPos arr func = concatMap2DArrayWithIndex arr (\colIndex rowIndex elem -> func (colIndex, rowIndex) elem)

concatMapWithIndex :: (Int -> a -> [b]) -> [a] -> [b]
concatMapWithIndex f xs = concat $ zipWith f [0..] xs

mapWithIndex :: (Num a, Enum a) => (a -> b -> c) -> [b] -> [c]
mapWithIndex f xs = zipWith f [0..] xs

containsAll :: Eq a => [a] -> [a] -> Bool
containsAll subList mainList = all (`elem` mainList) subList

intToAscii :: Int -> String
intToAscii n = [chr n]
