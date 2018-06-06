-- Julian Kotrba
-- 01427123

module AufgabeFFP6 where

import Data.List

run :: Bunch m => Pred m -> m Answer
run p = p initial

initial :: Answer
initial = MkAnswer (idsubst,0)

data Term = Int Int
	| Nil
	| Cons Term Term
  | Var Variable deriving Eq

data Variable = Named String
	| Generated Int deriving (Show, Eq)

type Pred m = Answer -> m Answer

newtype Answer = MkAnswer (Subst, Int)

type Stream a = [a]

newtype Matrix a = MkMatrix (Stream [a]) deriving Show

unMatrix (MkMatrix xm) = xm

newtype Diag a = MkDiag (Stream a) deriving Show

unDiag (MkDiag xs) = xs

instance Monad Diag where
   return x = MkDiag [x]
   MkDiag xs >>= f = MkDiag (concat (diag (map (unDiag . f) xs)))

diag :: Stream (Stream a) -> Stream [a]
diag [] = []
diag (xs:xss) = lzw (++) [[x] | x <- xs] ([] : diag xss)

lzw :: (a -> a -> a) -> [a] -> [a] -> [a]
lzw f [] ys = ys
lzw f xs [] = xs
lzw f (x:xs) (y:ys) = f x y : lzw f xs ys

list :: [Int] -> Term
list xs = foldr Cons Nil (map Int xs)

var :: String -> Term
var s = Var (Named s)

class Monad m => Bunch m where
	zero :: m a
	alt :: m a -> m a -> m a
	wrap :: m a -> m a

instance Bunch [] where
	zero = []
	alt xs ys = xs ++ ys
	wrap xs = xs

instance Bunch Diag where
	zero = MkDiag []
	alt (MkDiag xs) (MkDiag ys) = MkDiag (shuffle xs ys)
	wrap xm = xm


instance Monad Matrix where
	return x = MkMatrix [[x]]
	(MkMatrix xm) >>= f = MkMatrix (bindm xm (unMatrix . f))

instance Bunch Matrix where
	zero = MkMatrix []
	alt ( MkMatrix xm) (MkMatrix ym) = MkMatrix (lzw (++) xm ym)
	wrap (MkMatrix xm) = MkMatrix ([]:xm)

intMat = MkMatrix [[n] | n <- [1..]]

bindm :: Stream [a] -> (a -> Stream [b]) -> Stream [b]
bindm xm f = map concat (diag (map (concatAll . map f) xm))

concatAll :: [Stream [b]] -> Stream [b]
concatAll = foldr (lzw (++)) []

shuffle :: [a] -> [a] -> [a]
shuffle [] ys =ys
shuffle (x:xs) ys = x:shuffle ys xs

(=:=) :: Bunch m => Term -> Term -> Pred m
(t =:= u) (MkAnswer (s,n)) =
	case unify (t, u) s of
       	Just s' -> return (MkAnswer(s',n))
       	Nothing -> zero

(&&&) :: Bunch m => Pred m -> Pred m -> Pred m
(p &&& q) s = p s >>= q

(|||) :: Bunch m => Pred m -> Pred m -> Pred m
(p ||| q) s = alt (p s) (q s)

infixr 4 =:=
infixr 3 &&&
infixr 2 |||

-- changed Var to Variable
newtype Subst = MkSubst [(Variable, Term)]
unSubst(MkSubst s) = s

good ::  Bunch m => Term -> Pred m
good (s) =
  step (s =:= Cons (Int 0) Nil
   ||| exists (\t -> exists (\q -> exists (\r ->
        s =:= Cons (Int 1) t
        &&& append (q,r,t)
        &&& good (q)
        &&& good (r)))))

join s [] = ""
join s [x] = x
join s (x:xs) = x ++ s ++ join s xs

idsubst = MkSubst[]
extend x t (MkSubst s) = MkSubst ((x,t):s)

apply :: Subst -> Term -> Term
apply s t =
	case deref s t of
        Cons x xs -> Cons (apply s x) (apply s xs)
        t'        -> t'

deref :: Subst -> Term -> Term
deref s (Var v) =
	case lookup v (unSubst s) of
        Just t    -> deref s t
        Nothing   -> Var v
deref s t = t

unify :: (Term, Term) -> Subst -> Maybe Subst
unify (t,u) s =
	case (deref s t, deref s u) of
      	(Nil, Nil) -> Just s
      	(Cons x xs, Cons y ys)  -> unify (x,y) s >>= unify (xs, ys)
      	(Int n, Int m) | (n==m) -> Just s
      	(Var x, Var y) | (x==y) -> Just s
      	(Var x, t)              -> if occurs x t s then Nothing
                                       else Just (extend x t s)
      	(t, Var x)              -> if occurs x t s then Nothing
                                       else Just (extend x t s)
      	(_,_)                   -> Nothing

occurs :: Variable -> Term -> Subst -> Bool
occurs x t s =
	case deref s t of
      	Var y     -> x == y
      	Cons y ys -> occurs x y s || occurs x ys s
      	_         -> False


append :: Bunch m => (Term, Term, Term) -> Pred m
append(p,q,r) =
	step(p =:= Nil &&& q =:= r
        ||| exists (\x -> exists (\a -> exists (\b ->
        	p =:= Cons x a &&& r =:= Cons x b
               	&&& append(a,q,b)))))

step :: Bunch m => Pred m -> Pred m
step p s = wrap(p s)

exists :: Bunch m => (Term -> Pred m) -> Pred m
exists p (MkAnswer (s,n)) =
    p (Var (Generated n)) (MkAnswer (s,n+1))

-- implementation

instance Show Subst where
   show (s @ (MkSubst vts)) =
		 "{" ++
		 join "," (map (\v -> v ++ "=" ++ show (apply s (Var (Named v)))) (variables_from_subs vts)) ++
		 "}"

variables_from_subs :: [(Variable, Term)] -> [String]
variables_from_subs vts = sort [v | (Named v,t) <- vts]

instance Show Term where show t = show_t1 t

show_t1 :: Term -> String
show_t1 (Int n) = show n
show_t1 (Cons t1 t2) = "[" ++ show_t1 t1 ++ show_t2 t2 ++ "]"
show_t1 (Var (Named s)) = show s
show_t1 (Var (Generated i)) = show i
show_t1 Nil = "Nil"

show_t2 :: Term -> String
show_t2 (Cons t1 t2) = "," ++ show_t1 t1 ++ show_t2 t2
show_t2 Nil = ""

-- prevent from printing constructor MkAnswer s n
-- show just s
instance Show Answer where show (MkAnswer (s,n)) = show s

shuffleLP :: Bunch m => (Term, Term, Term) -> Pred m
shuffleLP (p,q,r)
	= step(
				p =:= Nil &&& q =:=r
				|||
				q =:= Nil &&& p =:= r
				|||
				exists (\x ->
 					 r =:= list (shuffle (t2a p) (t2a q))  )
			)

t2a :: Term -> [Int]
t2a (Var a) = []
t2a (Int a) = [a]
t2a Nil = []
t2a (Cons a b) = t2a a ++ t2a b

regSeq :: Bunch m => Term -> Pred m
regSeq (s)
	= step (
		s =:= Cons (Int 1) Nil
		|||
		s =:= Cons (Int 2) Nil
		|||
		s =:= Cons (Int 3) Nil
		||| (exists (\t ->
			s =:= Cons (Int 1) t &&& regSeqAfterAtom (t)
			|||
			s =:= Cons (Int 2) t &&& regSeqAfterAtom (t)
			|||
			s =:= Cons (Int 3) t &&& regSeqAfterAtom (t)
			)
		)
	)

regSeqAfterZero :: Bunch m => Term -> Pred m
regSeqAfterZero (s)
	= step (
		s =:= Cons (Int 1) Nil
		|||
		s =:= Cons (Int 2) Nil
		|||
		s =:= Cons (Int 3) Nil
 		|||
		(exists (\t ->
			s =:= Cons (Int 1) t &&& regSeqAfterAtom (t)
			|||
			s =:= Cons (Int 2) t &&& regSeqAfterAtom (t)
			|||
			s =:= Cons (Int 3) t &&& regSeqAfterAtom (t)
			)
		)
	)

regSeqAfterAtom :: Bunch m => Term -> Pred m
regSeqAfterAtom (s)
	= step (
		s =:= Cons (Int 1) Nil
		|||
		s =:= Cons (Int 2) Nil
		|||
		s =:= Cons (Int 3) Nil
	 	||| (exists (\t ->
			s =:= Cons (Int 0) t &&& regSeqAfterZero (t)
			|||
			s =:= Cons (Int 1) t &&& regSeqAfterAtom (t)
			|||
			s =:= Cons (Int 2) t &&& regSeqAfterAtom (t)
			|||
			s =:= Cons (Int 3) t &&& regSeqAfterAtom (t)
			)
		)
	)
