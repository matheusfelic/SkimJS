import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map
import Debug.Trace
import Value

-- Evalute For Init values
evalForInit :: StateT -> ForInit -> StateTransformer Value
evalForInit env NoInit = return Nil -- for(; anything);
evalForInit env (VarInit []) = return Nil --for([]; anything)
evalForInit env (VarInit (x:xs)) =
    varDecl env x >> evalForInit env (VarInit xs) -- for([1,2,3]; anything)
evalForInit env (ExprInit expr) = evalExpr env expr -- for(int x = 0; anything)


-- ////////////////////////////////////////////////////
-- ////////////////// Evaluate Expression /////////////
-- ////////////////////////////////////////////////////

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (StringLit str ) = return $ String str
evalExpr env (ArrayLit []) = return $ List []
evalExpr env (ArrayLit (x:xs)) = return $ List values
    where values = evalList env (x:xs) []
evalExpr env (DotRef expr id) = do
        case id of
            (Id "head") -> do
                list <- evalExpr env expr
                case list of
                    (List []) -> return $ List []
                    (List (x:xs)) -> return x
                    _ -> error ("Invalid Expression")
            (Id "tail") -> do
                list <- evalExpr env expr
                case list of
                    (List []) -> return $ List []
                    (List (x:xs)) -> return $ List xs
                    _ -> error ("Invalid Expression")
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (UnaryAssignExpr PostfixDec (LVar var)) = do
    v <- stateLookup env var
    case v of 
        (Error _) -> error("Variable "++ show var ++" Not Defined")
        (Int i) -> do
            setVar var (Int(i - 1))
        (Undeclared (Int i)) -> do
            setVar var (Int(i - 1))
evalExpr env (UnaryAssignExpr PostfixInc (LVar var)) = do
    v <- stateLookup env var
    case v of 
        (Error _) -> error("Variable "++ show var ++" Not Defined")
        (Int i)-> do
            setVar var (Int( i + 1))
        (Undeclared (Int i)) -> do
            setVar var (Int(i + 1))
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env var
    case v of
        -- Variable not defined :(
        (Error _) ->  do                  -- variavel global
            e <- evalExpr env expr
            setVar var (Undeclared e)
                                         
        -- Variable defined, let's set its value
        _ -> do
            e <- evalExpr env expr
            setVar var e
evalExpr env (CallExpr functionName paramsExpCall) = do
     result <- evalExpr env functionName
     case result of
        (Error _) -> error "Function not defined"
        (FunctionValue name params listaStmts) -> ST $ \s -> 
            
            let (ST f1) = mapM (evalExpr env) paramsExpCall                
               
                (paramsEval, _) = f1 s
                
                parameters = fromList (zip (Prelude.map (\(Id a) -> a) params) paramsEval)
                
                newS = union parameters s
                
                (ST g) = evalStmt env (BlockStmt listaStmts)

                (f, finalS) = g newS

                semParametros = (difference finalS parameters)

                varDaFunct = (difference semParametros s)
                --show varDaFunct
                globais =  Map.filter (
                    \x -> 
                        case x of 
                            (Undeclared z) -> True
                            _ -> False
                 ) varDaFunct  

            in (f, (union globais (union (intersection (difference finalS parameters) s) s)))
-- Funcao para transformar uma lista de expression em uma lista de value

evalList :: StateT -> [Expression] -> [Value] -> [Value]
evalList env [] list = list
evalList env (x:xs) list = evalList env xs postList
    where postList = list ++ [exprToValue env x]

-- Funcao para transformar Expression em Value
exprToValue :: StateT -> Expression -> Value
exprToValue env expr = getValue $ evalExpr env expr

-- Funcao para pegar o value do StateTransformer
getValue :: StateTransformer Value -> Value
getValue (ST f) = valor
    where (valor,estado) = f empty 

-- ////////////////////////////////////////////////////
-- ////////////////// Evaluate Statements /////////////
-- ////////////////////////////////////////////////////

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (ReturnStmt expression) = do
    case expression of
        (Nothing) -> return (Return Nil)
        (Just expr) -> do
            exprEval <- evalExpr env expr
            return (Return exprEval)
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (FunctionStmt f@(Id name) params statements) = setVar name (FunctionValue f params statements)

-- IfElse evalStmt quando avalia tru ou fals vai para o BlockStmt
evalStmt env (IfStmt expr tru fals) = do
    result <- evalExpr env expr 
    case result of
        (Bool b) -> if b then (evalStmt env tru) else (evalStmt env fals)
        error@(Error _) -> return error
-- If evalStmt quando avalia tru vai para o BlockStmt
evalStmt env (IfSingleStmt expr tru) = do
    result <- evalExpr env expr
    case result of
        (Bool b) -> if b then (evalStmt env tru) else return Nil 
        error@(Error _) -> return error

evalStmt env (BlockStmt []) = return Nil -- {... [] }
evalStmt env (BlockStmt ((BreakStmt Nothing):xs)) = return Break -- { break }
evalStmt env (BlockStmt (x:xs)) = do
    result <- evalStmt env x -- avalia o proximo stmt 
    case result of
        (Return a) -> return (Return a) -- {... return x}
        (Break) -> return Break -- {... break})
        _ -> evalStmt env (BlockStmt xs) -- chamada recursiva para avaliar o restante

evalStmt env (ForStmt initial maybeComparation maybeIncrement stmts) = do
    evalForInit env initial
    case maybeComparation of
        Nothing -> do
            a <-evalStmt env stmts 
            case a of
                Break -> return Break
                (Return val) -> return (Return val)
                _ -> case maybeIncrement of
                    Nothing -> evalStmt env (ForStmt NoInit Nothing Nothing stmts) 
                    (Just increment) -> evalExpr env increment >>  (evalStmt env (ForStmt NoInit Nothing maybeIncrement stmts))
        (Just comparation) -> do
            result <- evalExpr env comparation
            case result of
                (Bool b) -> if b then do
                   value <- evalStmt env stmts
                   case value of
                    Break -> return Break
                    (Return val) -> return (Return val) 
                    _ -> case maybeIncrement of
                        Nothing -> do
                             evalStmt env (ForStmt NoInit maybeComparation Nothing stmts)
                        (Just increment) -> do
                            evalExpr env increment
                            evalStmt env (ForStmt NoInit maybeComparation maybeIncrement stmts)
                else return Nil        

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env [stmt] = evalStmt env stmt
evaluate env (s:ss) = evalStmt env s >> evaluate env ss

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env op (Undeclared v1)  (Undeclared v2) = infixOp env op v1 v2
infixOp env op (Undeclared v1)  v2 = infixOp env op v1 v2
infixOp env op v1  (Undeclared v2) = infixOp env op v1 v2
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpAdd  (Return v1) (Return v2) = infixOp env OpAdd v1  v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2
infixOp env OpEq   (List []) (List []) = return $ Bool True
infixOp env OpEq   (List []) (List l) =  return $ Bool False
infixOp env OpEq   (List l) (List []) =  return $ Bool False
infixOp env OpEq   (List v1) (List v2) = do 
    b1 <- infixOp env OpEq (head v1) (head v2)
    b2 <- infixOp env OpEq (List (tail v1)) (List (tail v2))
    ans <- (infixOp env OpLAnd b1 b2)
    return ans
infixOp env OpNEq  (List list1) (List list2) = do
    (Bool notAns) <- infixOp env OpEq  (List list1) (List list2)
    return $ Bool $ not (notAns)
infixOp env OpAdd   (List v1) (List v2) = return $ List $ v1++v2

infixOp env op (Var x) v2 = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error 
        val -> infixOp env op val v2

infixOp env op v1 (Var x) = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error 
        val -> infixOp env op v1 val

infixOp env op _ _ = do 
     error "Function is not defined for these arguments"
--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    (maybe
        (Error $ "Variable " ++ show var ++ " not defined")
        id
        (Map.lookup var (union s env)),
    s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) = show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
