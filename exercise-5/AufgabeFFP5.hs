-- Julian Kotrba
-- 01427123

module AufgabeFFP5 where

--import Data.Char

type Parse0 a b = [a] -> [(b,[a])]

data Expr = Lit Int | Var Char | Op Ops Expr Expr deriving (Eq,Ord,Show)
data Ops = Add | Sub | Mul | Div | Mod deriving (Eq,Ord,Show)

topLevel :: Parse0 a b -> [a] -> b
topLevel p inp
	= case results of
	[] -> error "parse unsuccessful"
	_  -> head results
	where
	results = [ found | (found, []) <- p inp ]

parser :: Parse0 Char Expr
parser = nameParse `alt` litParse `alt` optExpParse

topLevelWSp :: Parse0 a b -> [a] -> b
topLevelWSp p inp
	= case results of
        [] -> error "parse unsuccessful"
        _  -> head results
        where
        results = [ found | (found, []) <- p inp ]

parserWSp :: Parse0 Char Expr
parserWSp a = (nameParse `alt` litParse `alt` optExpParse) (filter (\c -> (c /= ' ')) a)

-- primitives

-- return empty list
-- signals failure of the analysis
none :: Parse0 a b
none _ = []

-- return non empty list
-- signals success of the analysis
succeed :: b -> Parse0 a b
succeed val inp = [(val,inp)]

-- parser recognizes a singel object: t
token :: Eq a => a -> Parse0 a a
token t = spot (== t) -- easyier implementation because of spot

-- returns list of results or empty list
-- depending on f
spot :: (a -> Bool) -> Parse0 a a
spot p [] = []
spot p (x:xs)
	| p x = [(x, xs)]
	| otherwise = []


-- check if first symbol is a digit
dig :: Parse0 Char Char
dig = spot isDigit

isDigit :: Char -> Bool
isDigit ch = ('0'<=ch) && (ch<='9')


-- Parser combinators

-- alternative: combine the results of two parsers
-- expression is either literal, variable or operator
alt :: Parse0 a b -> Parse0 a b -> Parse0 a b
alt p1 p2 inp = p1 inp ++ p2 inp

-- (>*>): combine parsers sequentially 
-- eg. operator: bracket -> number 
infixr 5 >*>
(>*>) :: Parse0 a b -> Parse0 a c -> Parse0 a (b,c)
(>*>) p1 p2 inp
	= [((y,z),rem2) | (y,rem1) <- p1 inp,
			  (z,rem2) <- p2 rem1 ]

-- build: map like -> transform parser
-- eg. character to integer
build :: Parse0 a b -> (b -> c) -> Parse0 a c
build p f inp = [(f x, rem)|(x,rem) <- p inp]


-- helpers

wsParse :: Parse0 Char [Char]
wsParse (' ':xs) = [([], xs)]
wsParse a  = [([], a)] 

-- Parsing varibale names (a .. z)
nameParse :: Parse0 Char Expr
nameParse = spot (\c -> 'a' <= c && c <= 'z') `build` Var 

-- Parsing literals (numerals)
litParse :: Parse0 Char Expr
litParse = ((optional (token '~')) >*> (neList (spot isDigit))) `build` (charListToExpr . uncurry (++))


-- Parsing operator expressions
optExpParse :: Parse0 Char Expr
optExpParse
	= ( token '(' >*>
	parser >*>
	spot isOp >*>
	parser >*>
	token ')' )
	`build` makeExpr 

makeExpr (_,(e1,(op,(e2,_)))) = Op (charToOp op) e1 e2

charToOp :: Char -> Ops
charToOp '+' = Add
charToOp '-' = Sub
charToOp '*' = Mul
charToOp '/' = Div
charToOp '%' = Mod

isOp :: Char -> Bool
isOp c
	| c=='+' || c=='-' || c=='*' || c=='/' || c=='%' = True
	| otherwise = False


optional :: Parse0 a b -> Parse0 a [b]
optional p = (succeed [])
	`alt`
	(p  `build` (:[]))

neList :: Parse0 a b -> Parse0 a [b]
neList p = (p `build` (:[]))
	`alt` ((p >*> neList p) `build` (uncurry (:)))

charListToExpr :: [Char] -> Expr
charListToExpr = Lit . charListToInt 

charListToInt :: [Char] -> Int
charListToInt ('~':rest) = - (sToInt rest)
charListToInt other = sToInt other

sToInt :: String -> Int
sToInt = read 


-- part 2 

newtype Parser a = Parse (String -> [(a,String)])

instance Monad Parser where
p >>= f = Parse (\cs -> concat [(parse (f a)) cs' |
          (a,cs') <- (parse p) cs])

return a = Parse (\cs -> [(a,cs)])
	where
parse :: (Parser a) -> (String -> [(a,String)])
parse (Parse p) = p


item :: Parser Char
item = Parse (\cs -> case cs of
			"" ->[] 
			(c:cs) -> [(c,cs)])



