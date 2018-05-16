-- Julian Kotrba
-- 01427123

module AufgabeFFP5 where

type Parse0 a b = [a] -> [(b,[a])]

data Expr = Lit Int | Var Char | Op Ops Expr Expr deriving (Eq,Ord,Show)
data Ops = Add | Sub | Mul | Div | Mod deriving (Eq,Ord,Show)

parser :: Parse0 Char Expr
-- TODO

topLevel :: Parse0 a b -> [a] -> b
-- TODO
