data Prop
  = Con Bool
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Implies Prop Prop
  | Name Name
  deriving (Eq, Show)

type Name = String

-- If deMorgan isn't applicable, it returns the original proposition
deMorgan :: Prop -> Prop
deMorgan (Not (And p q)) = Or (Not p) (Not q)
deMorgan (Not (Or p q)) = And (Not p) (Not q)
deMorgan p = p

testDeMorgan :: Bool
testDeMorgan = deMorgan (Not (And (Con True) (Con False))) == Or (Not (Con True)) (Not (Con False))