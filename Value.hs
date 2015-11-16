module Value (Value (..)) where
import Language.ECMAScript3.Syntax  

data Value = Bool Bool
    | Int Int
    | String String
    | Var String
    | List [Value]
    | Error String
    | FunctionValue Id [Id] [Statement]
    | Return Value
    | Undeclared Value
    | Break
    | Nil

--
-- Pretty Printer
--

instance Show Value where 
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Int int) = show int
  show (String str) = "\"" ++ str ++ "\""
  show (Var name) = name
  show (List val) = show val
  show (Error str) = "Error: " ++ str
  show (Return a) = show a
  show (Undeclared a) = show a
  show (FunctionValue (Id name) params statemens) =  "Function " ++ name
  show Nil = ""
  show Break = "Break"
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [Value] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ " " ++ (showListContents as)
