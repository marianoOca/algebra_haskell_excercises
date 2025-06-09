import Data.Char (ord, chr)

ordI :: Char -> Integer      --con esto resuelvo el problema de los tipos de antemano
ordI c = fromIntegral (ord c)

chrI :: Integer -> Char
chrI i = chr (fromInteger i)

--funciones de clases anteriores--
menorDivisor :: Integer -> Integer -> Integer
menorDivisor n k | n `mod` k == 0 = k
                 | otherwise = menorDivisor n (k+1)

esPrimo :: Integer -> Bool
esPrimo n | n <= 1 = False
          | otherwise = n == menorDivisor n 2

menorPrimo :: Integer -> Integer
menorPrimo n | esPrimo n = n
             | otherwise = menorPrimo (n+1)

nesimoPrimo :: Integer -> Integer                 --este lo uso para encontrar primos más facilmente
nesimoPrimo 1 = 2
nesimoPrimo n = menorPrimo (1 + nesimoPrimo (n-1))


mcd :: Integer -> Integer -> Integer
mcd a 0 = abs a
mcd a b = mcd b (mod a b)
--{-
emcd :: Integer -> Integer -> (Integer,Integer,Integer)
emcd a 0 | a > 0 = (a, 1, 0)
         | otherwise = (abs a, -1, 0)
emcd a b = (d, t', s' - t'*q)
 where q = div a b
       r = mod a b
       (d, s', t') = emcd b r


ecEquivalente :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
                where d = mcd a m

solucionEcConPropAdic :: (Integer, Integer, Integer) -> Integer
solucionEcConPropAdic (a, b, m) = mod (s*b) m
                where (d, s, t) = emcd a m

solucionEc :: (Integer, Integer, Integer) -> Integer
solucionEc e = solucionEcConPropAdic (ecEquivalente e)
--}

--1--
encontrarGranE :: Integer -> Integer -> Integer
encontrarGranE e c | mcd e c == 1 = c                  --esto es equivalente a preguntar si sonCoprimos e c
                   | otherwise = encontrarGranE e (c-1)

encontrarMiniE :: Integer -> Integer -> Integer
encontrarMiniE e c | mcd e c == 1 = c
                   | otherwise = encontrarMiniE e (c+1)

encontrarD :: Integer -> Integer -> Integer -> Integer --como e*d congruente 1 modulo m  es igual a m | e*d-1 = solucionEc (e,1,m)
encontrarD e m d | mcd e m /= 1 = 0                    --cuido que el sistema no sea incompatible
                 | mod (e*d - 1) m == 0 = d
                 | otherwise = encontrarD e m (d+1)

generarClaves :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer)) -- ((n, clave pública), (n, clave privada))
generarClaves p q | n <= 127 = ((0,0),(0,0))                                    -- esto indica que los primos son muy chicos
                  | not (esPrimo p) || not (esPrimo q) = ((1,1),(1,1))          -- esto indica que p y/o q no es primo
                  | otherwise = ((n,d), (n,e))
          where n = p*q
                m = (p-1)*(q-1)
                e = encontrarGranE m (m-3)  -- encontrarMiniE m 2               --si se quiere un e más chico
                d = encontrarD e m 1  --solucionEc (e,1,m)                      --es la opción "a la antigua"


--2--
encriptar :: (Integer, Integer) -> String -> [Integer]  -- (n, d = clave pública)
encriptar _ [] = []
encriptar (n,d) (h:t) = (mod ((ordI h)^d) n):(encriptar (n,d) t)


--3--
encontrarA :: Integer -> Integer -> Integer -> Integer -> Integer  -- resto de (b^e)/n = a es igual a b^e congruente a modulo n
encontrarA b n e a = solucionEc (a, b^e, n)                        -- (esto es más rápido)            n | b^e-a
                 {-| mod (b^e - a) n == 0 = a                                           
                   | otherwise = encontrarA b n e (a+1) --}        -- (pero esto es más simple)

desencriptar :: (Integer, Integer) -> [Integer] -> String  -- (d, e = clave privada)
desencriptar _ [] = []
desencriptar (n,e) (b:t) = (chrI a):(desencriptar (n,e) t)
        where a = encontrarA b n e 1


--4a--
sirveE :: (Integer, Integer) -> [Integer] -> Bool  -- (d, e = clave privada)
sirveE _ [] = True
sirveE (n,e) (b:t) | a <= 127 = sirveE (n,e) t
                   | otherwise = False
        where a = encontrarA b n e 1

encontrarE :: Integer -> [Integer] -> Integer-> Integer  -- tengo que encontrar e / a <= 127
encontrarE n l e | sirveE (n,e) l = e
                 | otherwise = encontrarE n l (e+1)

rompeCodigos :: (Integer, Integer) -> [Integer] -> String -- demora: 8 min 6 seg
rompeCodigos (n,_) l = desencriptar (n, encontrarE n l 1) l


--4b--
factorizarDesde :: Integer -> Integer -> Integer
factorizarDesde n c | (mod n p == 0) && esPrimo (div n p) = p
                    | otherwise = factorizarDesde n (c+1)
            where p = nesimoPrimo c

tirameUnaE :: Integer -> Integer -> Integer-> Integer  -- aprovecho que m | e*d - 1 , m = (p-1)*(q-1) y n = p*q
tirameUnaE d m e = solucionEc (d,1,m)                  -- (esto es más rápido)
               {-| mod (e*d - 1) m == 0 = e
                 | otherwise = tirameUnaE d m (e+1) --}-- (pero esto es más simple)

rompeCodigosV2 :: (Integer, Integer) -> [Integer] -> ((Integer, Integer), String)  -- demora: < 1 seg
rompeCodigosV2 (n,d) l = ( (n,e), desencriptar (n, e) l )
               where e = tirameUnaE d m 1
                     m = (p-1)*(q-1)
                     p = factorizarDesde n 1
                     q = div n p



--(100337, 60953) [33706,38913,58255,99961,77756,23881,220,77756,1606,38913,77756,78982,18800,91658,91658,58255,77756,96593,58255,438,22839,28700,18800,1606,58255,48389]
--(100337, 1001) Sería imposible elejir solo una.


--Ahora me toca a mi! te reto a desencriptar el siguiente mensaje (te dejo 2 con la misma clave por si es más fácil):
--Clave pública: (18749053,6246793),(18749053,18740377)

--[637320,2814143,11816248,211447,13959085,1814241,9166233,11816248,211447,18533147,16127925,211447,1814241,211447,6390649,18533147,11816248]
--[9521659,14322649,1814241,13959085,18533147,1814241,16127925,211447,7207121,1814241,829528,18533147,211447,15986769,6035226,14322649,211447,6390649,1814241,211447,11816248,174222,11816248,6390649,11816248,829528,637320,11816248,211447,13959085,18042834,14322649,16127925,1814241,6844201,1814241,2934977,2934977]