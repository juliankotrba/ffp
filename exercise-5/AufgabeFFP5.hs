-- Julian Kotrba
-- 01427123

module AufgabeFFP5 where

import Data.Char
import Control.Monad

-- Part 1

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

--isDigit :: Char -> Bool
--isDigit ch = ('0'<=ch) && (ch<='9')

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
charListToInt ('~':rest) = - (read rest)
charListToInt other = read other

-- Part 2

newtype Parser a = Parse (String -> [(a,String)])

instance Monad Parser where
	p >>= f = Parse (\cs -> concat [(parse (f a)) cs' |
        	(a,cs') <- (parse p) cs])

	return a = Parse (\cs -> [(a,cs)])

instance MonadPlus Parser where
	mzero = Parse (\cs -> [])
	p `mplus` q = Parse (\cs -> parse p cs ++ parse q cs)

parse :: (Parser a) -> (String -> [(a,String)])
parse (Parse p) = p

item :: Parser Char
item = Parse (\cs -> case cs of
			"" ->[]
			(c:cs) -> [(c,cs)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parse (\cs -> case parse (p `mplus` q) cs of
	[] -> []
	(x:xs) -> [x])

char :: Char -> Parser Char
char c = sat (c ==)

space :: Parser String
space = many (sat (\c -> c == ' '))

token2 :: Parser a -> Parser a
token2 p = do {a <- p; space; return a}

sat  :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do a <- p; as <- many p; return (a:as)

symb :: String -> Parser String
symb cs = token2 (string cs)

apply  :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

expr :: Parser Int
expr = term `chainl1` addop

term = factor `chainl1` mulop

factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}

addop :: Parser (Int -> Int -> Int)
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}

digit = do {x <- token2 (sat isDigit); return (ord x - ord '0')}

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where
    rest a = (do
      f <- op
      b <- p
      rest (f a b)) +++ return a

-- Part 3

newtype Parser2 a b = Parse2 ([a] -> [(b,[a])])

instance Monad (Parser2 a) where
	p >>= f = Parse2 (\cs -> concat [(parse2 (f a)) cs' |
        	(a,cs') <- (parse2 p) cs])

	return a = Parse2 (\cs -> [(a,cs)])

instance MonadPlus (Parser2 a) where
	mzero = Parse2 (\cs -> [])
	p `mplus` q = Parse2 (\cs -> parse2 p cs ++ parse2 q cs)

parse2 :: (Parser2 a b) -> ([a] -> [(b,[a])])
parse2 (Parse2 p) = p

item2 :: Parser2 a a
item2 = Parse2 (\cs -> case cs of
			[] ->[]
			(c:cs) -> [(c,cs)])

(++++) :: Parser2 a b -> Parser2 a b -> Parser2 a b
p ++++ q = Parse2 (\cs -> case parse2 (p `mplus` q) cs of
	[] -> []
	(x:xs) -> [x])

char2 :: Char -> Parser2 Char Char
char2 c = sat2 (c ==)

space2 :: Parser2 Char String
space2 = many2 (sat2 (\c -> c == ' '))

token3 :: Parser2 Char b -> Parser2 Char b
token3 p = do {a <- p; space2; return a}

sat2  :: (a -> Bool) -> Parser2 a a
sat2 p = do {c <- item2; if p c then return c else mzero}

string2 :: String -> Parser2 Char String
string2 "" = return ""
string2 (c:cs) = do {char2 c; string2 cs; return (c:cs)}

many2 :: Parser2 a b -> Parser2 a [b]
many2 p = many12 p ++++ return []

many12 :: Parser2 a b -> Parser2 a [b]
many12 p = do a <- p; as <- many2 p; return (a:as)

symb2 :: String -> Parser2 Char String
symb2 cs = token3 (string2 cs)

apply2  :: Parser2 Char b -> String -> [(b,String)]
apply2 p = parse2 (do {space2; p})

expr2 :: Parser2 Char Int
expr2 = term2 `chainl12` addop2

term2 = factor2 `chainl12` mulop2

factor2 = digit2 ++++ do {symb2 "("; n <- expr2; symb2 ")"; return n}

addop2 :: Parser2 Char (Int -> Int -> Int)
addop2 = do {symb2 "+"; return (+)} ++++ do {symb2 "-"; return (-)}

mulop2 :: Parser2 Char (Int -> Int -> Int)
mulop2 = do {symb2 "*"; return (*)} ++++ do {symb2 "/"; return (div)}

digit2 = do {x <- token3 (sat2 isDigit); return (ord x - ord '0')}

chainl2 :: Parser2 a b -> Parser2 a (b -> b -> b) -> b -> Parser2 a b
chainl2 p op a = (p `chainl12` op) ++++ return a

chainl12 :: Parser2 a b -> Parser2 a (b -> b -> b) -> Parser2 a b
p `chainl12` op = do
  a <- p
  rest a
  where
    rest a = (do
      f <- op
      b <- p
      rest (f a b)) ++++ return a
