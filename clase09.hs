--1--
digitos :: Integer -> Integer -> [Integer]
digitos 0 _ = []
digitos n b = (mod n b):(digitos (div n b) b)

--2--
numero :: [Integer] -> Integer -> Integer
numero [] _ = 0
numero (h:t) b = h + b*(numero t b)

--6--
mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

--8--
mcm :: Integer -> Integer -> Integer
mcm a b = mod (mcd a b) (a*b)

--9--
emcd :: Int -> Int -> (Int,Int,Int)
emcd a 0 | a > 0 = (a, 1, 0)
         | otherwise = (abs a, -1, 0)
emcd a b = (d, t', s' - t'*q)
 where q = div a b
       r = mod a b
       (d, s', t') = emcd b r

--10--
emcd2 :: Int -> Int -> (Int, Int)
emcd2 a 0 | a > 0 = (1,0)
          | otherwise = undefined
emcd2 a b = (mod s' (div b d), t' + (div a d)*(div s' (div b d))
  where (d, s', t') = emcd a b

{-
a*s'+ b*t' = d

a*s + b*t = d

s=s' - (b/d) *k
t=t' + (a/d) *k

s + (b/d)*k = s'
-}