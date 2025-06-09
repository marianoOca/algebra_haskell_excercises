sumatoria :: [Int] -> Int
sumatoria l | l == [] = 0
            | otherwise = head l + sumatoria (tail l)

longitud :: [Int] -> Int
longitud l | l == [] = 0
           | otherwise = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | otherwise = (x == head l) || pertenece x (tail l)

primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 l | mod (head l) 45345 == 0 = (head l)
                        | otherwise = primerMultiplode45345 (tail l)

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

longitudPM :: [a] -> Int
longitudPM [] = 0
longitudPM (_:xs) = 1 + longitudPM xs

pertenecePM :: Int -> [Int] -> Bool
pertenecePM _ [] = False
pertenecePM x (h:t)  = (x == h) || pertenece x t

--01--
productoria :: [Int] -> Int
productoria [] = 1
productoria (h:t) = h * productoria t

--02--
sumarN :: Int -> [Int] -> [Int]
sumarN n l | tail l == [] = [head l + n]
           | otherwise = (head l + n) : sumarN n (tail l)

--03--
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (h:t) = sumarN h (h:t)

--04--
suElUl :: [Int] -> [Int] -> [Int]
suElUl (h:t) l | t == [] = sumarN h l
               | otherwise = suElUl t l

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo l = suElUl l l

--05--
pares :: [Int] -> [Int]
pares [] = []
pares (h:t) | h `mod` 2 == 0 = h : pares t
            | otherwise = pares t

--06--
quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar n (h:t) | h == n = t
               | otherwise = h : quitar n t

--07--
quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n (h:t) | h == n = quitarTodas n t
               | otherwise = h : quitarTodas n t

--08--
estaRep :: Int -> [Int] -> Bool
estaRep _ [] = False
estaRep n (h:t) = n == h || estaRep n t

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (h:t) = estaRep h t || hayRepetidos t

--09--
eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (h:t) = h : eliminarRepetidosAlFinal (quitarTodas h t)

--10--
eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = []
eliminarRepetidosAlInicio (h:t) | estaRep h t = eliminarRepetidosAlInicio t
                                | otherwise = h : eliminarRepetidosAlInicio t

--11--
esMayor :: Int -> [Int] -> Bool
esMayor _ [] = True
esMayor m (h:t) | m < h = False
                | otherwise = esMayor m t

maximo :: [Int] -> Int
maximo (h:t) | esMayor h t = h
             | otherwise = maximo t

--12--
esMenor :: Int -> [Int] -> Bool
esMenor _ [] = True
esMenor m (h:t) | m > h = False
                | otherwise = esMenor m t

orden :: [Int] -> [Int] ->[Int]
orden [] _ = []
orden (h:t) l | esMenor h t = h : (orden (quitar h l) (quitar h l))
              | otherwise = orden t l

ordenar :: [Int] -> [Int]
ordenar l = orden l l

--13--
darVuelta :: [Int] -> [Int] -> [Int]
darVuelta [] l2 = l2
darVuelta l1 l2 = darVuelta (tail l1) ((head l1):l2)

reverso :: [Int] -> [Int]
reverso l = darVuelta l []

--14-- [_,_]++[_,_]
enganchar :: [Int] -> [Int] -> [Int]
enganchar [] l2 = l2
enganchar (h:t) l2 = enganchar (t) ((h):l2)

concatenar :: [Int] -> [Int] -> [Int]
concatenar l1 l2 = enganchar (reverso l1) l2

concatenar2 :: [Int] -> [Int] -> [Int]
concatenar2 [] ys = ys
concatenar2 (x:xs) y = x: concatenar2 xs ys

--15-- zip [_,_] [_,_]
zipi :: [a] -> [b] -> [(a,b)]
zipi [] _ = []
zipi _ [] = []
zipi (ha:ta) (hb:tb) = (ha,hb):(zipi ta tb)

--extra-- Verdadero si la tercera es 0
extra :: [Int] -> Bool
extra (h1:(h2:(0:s)))) = True
extra _ = False