import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map
import Debug.Trace
import Value

-- ////////////////////////////////////////////////////
-- ////////////////// Evaluate Expression /////////////
-- ////////////////////////////////////////////////////

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (StringLit str ) = return $ String str
evalExpr env (ArrayLit []) = return $ Nil
evalExpr env (ArrayLit (x:xs)) = return $ List values
    where values = evalList env (x:xs) []
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env var
    case v of
        -- Variable not defined :(
       -- (Error _) -> return $ Error $ (show var) ++ " not defined"
        -- Variable defined, let's set its value
        _ -> do
            e <- evalExpr env expr
            setVar var e
evalExpr env (CallExpr functionName paramsExpCall) = do
     result <- evalExpr env functionName
     case result of
        (Error _) -> error "Function not defined"
        (FunctionValue name params listaStmts) -> ST $ \s -> -- s eh o estado externo
            -- avalia todos os parametros passados na chamada da funcao

            let (ST f1) = mapM (evalExpr env) paramsExpCall                  -- parametros avaliados eh o resultado
                -- parametros avaliados eh o resultado da aplicacao de b com o estado externo
                (paramsEval, _) = f1 s
                -- zip dos paarmetros de chamada avaliados com os parametros criados na declaracao da funcao
                parameters = fromList (zip (Prelude.map (\(Id a) -> a) params) paramsEval)
                -- novo estado eh a uniao do estado antigo com as variaveis
                newS = union parameters s
                -- com o novo estado podemos avaliar o bloco de statements

                -- // VARRER O BLOCO DE STMT E SE TIVER VARIAVEIS NAO DECLARADAS CONSIDERAR GLOBAIS E ADICIONAR AO ESTADO GLOBAL 's'
                (ST g) = evalStmt env (BlockStmt listaStmts)

                (f, finalS) = g newS

            in (f, (union (intersection (difference finalS parameters) s) s))
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
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (FunctionStmt f@(Id name) params statements) = setVar name (FunctionValue f params statements)

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env [stmt] = evalStmt env stmt
evaluate env (s:ss) = evalStmt env s >> evaluate env ss

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
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
