import Data.List (lookup, nub)
import Data.Maybe (fromMaybe)

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
  deriving (Show, Eq)

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
check env (Implies p1 p2) = not (check env p1) || check env p2

getElemsInSet :: Set -> [Set]
getElemsInSet (S elems) = elems

vonNeumann :: Int -> TERM v
vonNeumann 0 = Empty
vonNeumann n = Union (vonNeumann (n-1)) (Singleton (vonNeumann (n-1)))

claim1 :: Eq v => TERM v -> TERM v -> Bool
claim1 n1 n2 = not (showVonNeumann n1 <= showVonNeumann n2) || check [] (Subset n1 n2) 

claim2 :: Eq v => TERM v -> [Int]
claim2 n = map (length . getElemsInSet) (getElemsInSet (eval [] n))

showVonNeumann :: Eq v => TERM v -> Int
showVonNeumann n = length $ getElemsInSet $ eval [] n


-- testing below
testEnv :: Env String Set
-- testEnv = [("x",S (S ( S []))),("y", S [])]
testEnv = [("x", S [S [S []], S []]), ("y", S [S []])]

testSyntax :: TERM String
testSyntax = Intersection (Var "x") (Var "y")

testElem = check testEnv (Elem (Var "x") (Var "y"))

testSubset = check testEnv (Subset (Var "y") (Var "x"))

testMax = check testEnv (Implies (Elem (Var "y") (Var "x")) (Subset (Var "y") (Var "x")))
