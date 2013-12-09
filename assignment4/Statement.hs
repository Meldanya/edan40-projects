module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement = Assignment String Expr.T
                | Skip
                | Block [Statement]
                | If Expr.T Statement Statement
                | While Expr.T Statement
                | Read String
                | Write Expr.T
                deriving Show

parseAssignment = word #- accept ":=" #
                  Expr.parse #-
                  require ";" >-> buildAss
buildAss (var, expr) = Assignment var expr

parseSkip = accept "skip" #
            require ";" >-> buildSkip
buildSkip _ = Skip

parseBlock = accept "begin" -#
             iter parseStmt #-
             require "end" >-> Block

parseIf = accept "if" -# Expr.parse #
          require "then" -# parseStmt #
          require "else" -# parseStmt >-> buildIf
buildIf ((expr, s1), s2) = If expr s1 s2

parseWhile = accept "while" -# Expr.parse #
             require "do" -# parseStmt >-> buildWhile
buildWhile (expr, stmt) = While expr stmt

parseRead = accept "read" -#
            word #-
            require ";" >-> Read

parseWrite = accept "write" -#
             Expr.parse #-
             require ";" >-> Write

parseStmt = parseAssignment ! parseSkip ! parseBlock !
            parseIf ! parseWhile ! parseRead ! parseWrite

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict i =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict i
    else exec (elseStmts: stmts) dict i

exec (Assignment s e:stmts) dict i = exec stmts newDict i
    where newDict = Dictionary.insert (s, Expr.value e dict) dict

exec (Skip:stmts) dict i = exec stmts dict i
exec (Block s:stmts) dict i = exec (s ++ stmts) dict i
exec (While e s:stmts) dict i
    | Expr.value e dict > 0 = exec (s : While e s : stmts) dict i
    | otherwise             = exec stmts dict i

exec (Read s:stmts) dict (i:is) = exec stmts newDict is
    where newDict = Dictionary.insert (s,i) dict

exec (Write e:stmts) dict i = Expr.value e dict : exec stmts dict i


-- Unfortunately, toString became rather ugly when I decided to print the
-- program with the correct indentation...
indentify identlvl stmt = replicate identlvl '\t' ++ disp identlvl stmt

disp :: Int -> Statement -> String
disp i (Assignment s e) = s ++ " := " ++ Expr.toString e ++ "\n"
disp i (Skip)           = "skip;\n"
disp i (Block stmts)    = "begin\n" ++
                          concat (map (indentify (i+1)) stmts) ++
                          replicate i '\t' ++ "end\n"
disp i (If e s1 s2)     = "if " ++ Expr.toString e ++
                          " then\n" ++ indentify (i+1) s1 ++
                          replicate i '\t' ++ "else\n" ++ indentify (i+1) s2
disp i (While e s)      = "while " ++ Expr.toString e ++
                          " do\n" ++ indentify (i+1) s
disp i (Read s)         = "read " ++ s ++ ";\n"
disp i (Write e)        = "write " ++ Expr.toString e ++ ";\n"

instance Parse Statement where
  parse = parseStmt
  toString = indentify 0
