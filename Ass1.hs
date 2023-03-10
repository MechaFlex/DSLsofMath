import Data.List (lookup, nub, sort)
import Data.Maybe (fromMaybe)

-- Group A1.04
-- Jacob Bengtsson, Agnes Brogeby

data TERM v
  = Empty
  | Singleton (TERM v)
  | Union (TERM v) (TERM v)
  | Intersection (TERM v) (TERM v)
  | Var v

data PRED v
  = Elem (TERM v) (TERM v)
  | Subset (TERM v) (TERM v)
  | Not (PRED v)
  | And (PRED v) (PRED v)
  | Or (PRED v) (PRED v)
  | Implies (PRED v) (PRED v)

newtype Set = S [Set]
  deriving (Show, Ord)

instance Eq Set where
  (==) a b = formatSet a `setEqual` formatSet b
    where setEqual :: Set -> Set -> Bool
          setEqual (S xs) (S ys) = xs == ys   

formatSet :: Set -> Set
formatSet (S []) = S []
formatSet (S sets) = S (sort $ nub (map formatSet sets))

type Env var dom = [(var, dom)]

eval :: Eq v => Env v Set -> TERM v -> Set
eval _    Empty               = S []
eval env (Singleton s)        = S [eval env s]
eval env (Union s0 s1)        = unionSets (eval env s0) (eval env s1)
eval env (Intersection s0 s1) = intersectionSets (eval env s0) (eval env s1)
eval env (Var x)              = fromMaybe (error "Var not found") (lookup x env)

unionSets :: Set -> Set -> Set
unionSets (S x) (S y) = S (nub (x ++ y)) --nub removes duplicates

intersectionSets :: Set -> Set -> Set
intersectionSets (S x) (S y) = S [a | a <- x, a `elem` y]

check :: Eq v => Env v Set -> PRED v -> Bool
check env (Elem t1 t2)    = eval env t1 `elem` getElemsInSet (eval env t2)
check env (Subset t1 t2)  = and [ x `elem` getElemsInSet (eval env t2)
                                | x <- getElemsInSet (eval env t1)]
check env (Not p)         = not (check env p)
check env (And p1 p2)     = check env p1 && check env p2
check env (Or p1 p2)      = check env p1 || check env p2
check env (Implies p1 p2) = p1 ==> p2
  where (==>) p1 p2 = not (check env p1) || check env p2

getElemsInSet :: Set -> [Set]
getElemsInSet (S elems) = elems

vonNeumann :: Int -> TERM v
vonNeumann 0 = Empty
vonNeumann n = Union (vonNeumann (n-1)) (Singleton (vonNeumann (n-1)))

claim1 :: Int -> Int -> Bool
claim1 n1 n2 = not (n1 <= n2) || check emptyEnv (Subset (vonNeumann n1) (vonNeumann n2))

claim2 :: Int -> Bool
claim2 n = eval emptyEnv (vonNeumann n) == S [ eval emptyEnv (vonNeumann x) | x <- [0..(n-1)] ]

showVonNeumann :: Eq v => TERM v -> Int
showVonNeumann n = length $ getElemsInSet $ eval [] n

emptyEnv :: Env [()] v
emptyEnv = []


-- TESTING BELOW

testEnv :: Env String Set
-- testEnv = [("x",S (S ( S []))),("y", S [])]
testEnv = [("x", S [S [S []], S []]), ("y", S [S []])]

testSyntax :: TERM String
testSyntax = Intersection (Var "x") (Var "y")


--Test intersection
testIntersectionEnv1 :: Env String Set
testIntersectionEnv1 = [("x", S [S [S []], S []]), ("y", S [S []]), ("z" , S[S[S[S[S[]]]]])]

testIntersection :: Bool
testIntersection = eval testIntersectionEnv1 (Intersection (Var "x") (Var "x")) == eval testIntersectionEnv1 (Var "x")
testIntersection2 :: Bool
testIntersection2 = eval testIntersectionEnv1 (Intersection (Var "x") (Var "y")) /= eval testIntersectionEnv1 (Var "x")
testIntersection3 :: Bool
testIntersection3 = eval testIntersectionEnv1 (Intersection (Var "z") (Var "y")) == S []

testElem = check testEnv (Elem (Var "x") (Var "y"))

testSubset = check testEnv (Subset (Var "y") (Var "x"))

testMax = check testEnv (Implies (Elem (Var "y") (Var "x")) (Subset (Var "y") (Var "x")))

--Sikai

test0 :: Set
test0 = S [S [], S []]
test1 :: Set
test1 = S [S []]
test2 :: Set
test2 = S [test0, test1]
test3 :: Set
test3 = S [test1, test0]
test4 :: Set
test4 = S [test0, test1, test0]

test5 :: Set
test5 = S [test3]
test6 :: Set
test6 = S [test4]

test7 = S [test1, S [test1]]
test8 = S [S [test1], test1]

dup x y = Union (Singleton x) (Singleton y)
s0 = Empty
s1 = Singleton s0
a = dup s0 s1
b = dup s1 s0
ev = eval emptyEnv
testUn1 = ev a == ev b
testUn2 = ev (dup a b) == ev (Singleton a)