--01--
sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta _ 0 = 0
sumaDivisoresHasta n k | n `mod` k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)

--02--
sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

--03--
meDi :: Int -> Int -> Int
--meDi 1 _ = 1
meDi n k | n `mod` k == 0 = k
         | otherwise = meDi n (k+1)

menorDivisor :: Int -> Int
menorDivisor n = meDi n 2

--04--
esPrimo :: Int -> Bool
esPrimo n = n == menorDivisor n

--06--
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

meFaDe :: Int -> Int -> Int
meFaDe m k | m <= fact k = fact(k)
           | otherwise = meFaDe m (k+1)

menorFactDesde :: Int -> Int
menorFactDesde m = meFaDe m 1

--07--
maFaHa :: Int -> Int -> Int
maFaHa m k | m < fact k = fact(k-1)
           | otherwise = maFaHa m (k+1)

mayorFactHasta :: Int -> Int
mayorFactHasta m = maFaHa m 1

--08--
esFa :: Int -> Int -> Int
esFa n k | n == fact k = 1
         | n < fact k = 0
         | otherwise = esFa n (k+1)

esFact :: Int -> Bool
esFact n = esFa n 1 == 1

--09--
--esFibonacci :: Int -> Bool
--nuca entendi lo de fibonacci jaja

--10--
esSuInDePr :: Int -> Int -> Int
esSuInDePr _ 0 = 0
esSuInDePr n k | esPrimo k = k + esSuInDePr n (k-1)
               | otherwise = esSuInDePr n (k-1)

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = esSuInDePr n n == n

--11--
toVaMa :: Int -> Int -> Int -> Int
toVaMa n2 k m | k > n2 = m
              | sumaDivisores k > sumaDivisores m = toVaMa n2 (k+1) k
              | otherwise = toVaMa n2 (k+1) m

tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 = toVaMa n2 n1 n1

--12--
toVaMi :: Int -> Int -> Int -> Int
toVaMi n2 k m | k > n2 = m
              | sumaDivisores k < sumaDivisores m = toVaMi n2 (k+1) k
              | otherwise = toVaMi n2 (k+1) m

tomaValorMin :: Int -> Int -> Int
tomaValorMin n1 n2 = toVaMi n2 n1 n2

--13--
esSuDeDoPri :: Int -> Int -> Int -> Bool
esSuDeDoPri s1 s2 n | s1 > n = False
                    | esPrimo s1 && esPrimo s2 && ( s1 + s2 == n) = True
                    | esPrimo s1 && s2 /= n = esSuDeDoPri s1 (s2 + 1) n
                    | otherwise = esSuDeDoPri (s1 + 1) 2 n

esSumaDeDosPrimos :: Int -> Bool
esSumaDeDosPrimos n = esSuDeDoPri 2 2 n

--14--
goldbach :: Int -> Bool
goldbach n | n <= 2 || n `mod` 2 == 1 = undefined
           | otherwise = esSumaDeDosPrimos n

--15--
prGe :: Int -> Int -> Int -> Int -> Int
prGe n a b i | b > n = i
             | esPrimo b && esPrimo a && (b == a + 2) = prGe n 2 (b+1) (i+1)
             | esPrimo b && a < b = prGe n (a+1) b i
             | otherwise = prGe n 2 (b+1) i

primosGem :: Int -> Int
primosGem n = prGe n 2 2 0

--16--

proxPrimosGem :: Int -> (Int,Int)
proxPrimosGem n | esPrimo (n+1) && esPrimo (n+3) = (n+1,n+3)
                | otherwise = proxPrimosGem (n + 1)

--17a--
laSe :: Int -> Int -> Int
laSe a i | a == 1 = i
         | a `mod` 2 == 0 = laSe (a `div` 2) (i+1)
         | otherwise = laSe (3*a + 1) (i+1)

largoSecuencia :: Int -> Int
largoSecuencia a = laSe a 0

--17b--
roPi :: Int -> Int -> Int -> Int
roPi n k m | k == n = m
           | largoSecuencia k > largoSecuencia m = roPi n (k+1) k
           | otherwise = roPi n (k+1) m

rompePija :: Int -> Int
rompePija n = roPi n 1 1