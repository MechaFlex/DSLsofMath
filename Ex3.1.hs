type F2 a b c = (a,b) -> c

d :: (a -> b) -> (a -> b)
d = id

d1 :: F2 a b c -> F2 a b c
d1 f (a,b) = d (f . sndFixed b) a 

d2 :: F2 a b c -> F2 a b c
d2 f (a,b) = d (f . fstFixed a) b

fstFixed :: a -> (b -> (a,b))
fstFixed fix = \b -> (fix, b)

sndFixed :: b -> (a -> (a,b))
sndFixed fix = \a -> (a, fix)