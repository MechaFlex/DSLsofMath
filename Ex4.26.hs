import Data.Maybe (fromMaybe)

class Ring a where 
  zero :: a
  one :: a
  neg :: a -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data RingExp =
  Zero |
  One |
  Negate RingExp |
  Add RingExp RingExp |
  Mul RingExp RingExp |
  Var String
  deriving (Show)

instance Ring RingExp where
  zero = Zero
  one = One
  neg = Negate
  add = Add
  mul = Mul

instance Ring Double where
  zero = 0
  add = (+)
  neg = Prelude.negate
  one = 1
  mul = (*)

instance Ring Bool where
  zero = False
  add = (||)
  neg = not
  one = True
  mul = (&&)

eval :: Ring a => RingExp -> a
eval Zero = zero
eval One = one
eval (Negate re) = neg (eval re)
eval (Add reA reB) = add (eval reA) (eval reB)
eval (Mul reA reB) = mul (eval reA) (eval reB)
eval (Var s) = fromMaybe (error "Lookup failed") (lookup s table)

table :: Ring a => [(String, a)]
table = [("a",zero), ("b",one)]

re1, re2, re3 :: RingExp 
re1 = Negate (Var "b")

re2 = Mul (Add (Var "b") (Var "b")) (Add (Var "b") (Var "b"))

re3 = Add (Mul (Var "a") (Var "b")) (Mul Zero One)

main :: IO ()
main = do
  print (eval re1 :: Double, eval re1 :: Bool, eval re1 :: RingExp) 
  print (eval re2 :: Double, eval re2 :: Bool, eval re2 :: RingExp)
  print (eval re3 :: Double, eval re3 :: Bool, eval re3 :: RingExp)