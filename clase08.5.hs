menorDivisor :: Int -> Int -> Int
menorDivisor n k | n `mod` k == 0 = k
                 | otherwise = menorDivisor n (k+1)

esPrimo :: Int -> Bool
esPrimo n = not(n < 2) && n == menorDivisor n 2

menorPrimo :: Int -> Int
menorPrimo n | esPrimo n = n
             | otherwise = menorPrimo (n+1)

nesimoPrimo :: Int -> Int
nesimoPrimo 1 = 2
nesimoPrimo n = menorPrimo (1 + nesimoPrimo (n-1))

--1--
qPotenciaLoDivide :: Int -> Int -> Int
qPotenciaLoDivide n p | n `mod` p == 0  = 1 + qPotenciaLoDivide (n `div` p) p
                      | otherwise = 0

longitudDesde :: Int -> Int -> Int
longitudDesde 1 _ = 0
longitudDesde n k = 1 + (longitudDesde (n `div` (p^a)) (k+1))
          where p = nesimoPrimo k
                a = qPotenciaLoDivide n p

longitud :: Int -> Int
longitud n =  longitudDesde n 1

--2--
iesimo :: Int -> Int -> Int
iesimo n i  = qPotenciaLoDivide n (nesimoPrimo i)

--3--
headN :: Int -> Int
headN n = qPotenciaLoDivide n 2

--4--
correrPotencias :: Int -> Int -> Int
correrPotencias _ 1 = 1
correrPotencias n k = (pri^exp) * (correrPotencias n (k-1))
             where pri = nesimoPrimo (k-1)
                   exp = iesimo n k

tailN :: Int -> Int
tailN n = correrPotencias n (longitud n)

--5--
codificarDesde :: Int -> Int -> [Int]
codificarDesde 1 _ = []
codificarDesde n k =  (a):(codificarDesde (n `div` (p^a)) (k+1))
          where p = nesimoPrimo k
                a = qPotenciaLoDivide n p

codificarALista :: Int -> [Int]
codificarALista n = codificarDesde n 1

--6--
g :: [Int] -> Int -> Int
g [] _ = 1
g (h:t) k = ((nesimoPrimo k)^h) * (g t (k+1))

godel :: [Int] -> Int
godel l = g l 1