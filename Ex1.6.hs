f2p :: forall a b c. (a -> (b, c)) -> (a -> b, a -> c)
f2p f = (fst . f, snd . f)

p2f :: forall a b c. (a -> b, a -> c) -> (a -> (b, c))
p2f (fa, fb) = \x -> (fa x, fb x)

offsetInt :: Int -> (Int, Int)
offsetInt x = (x - 1, x + 1)

divideByTwo :: Int -> Int
divideByTwo = flip div 2

multiplyByTwo :: Int -> Int
multiplyByTwo = (2*)

main = do
  print $ fst (f2p offsetInt) 5
  print $ snd (f2p offsetInt) 5
  print $ p2f (divideByTwo, multiplyByTwo) 10