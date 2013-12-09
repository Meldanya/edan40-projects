module Program(T, parse, fromString, toString, exec) where

import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T] deriving (Show)

disp (Program stmts) = concat [ str | stmt <- stmts,
                                      let str = Statement.toString stmt,
                                      (not.null) str,
                                      str /= "\n" ]

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = disp

exec (Program s) = Statement.exec s Dictionary.empty
