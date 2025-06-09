type Set a = [a]

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

--a--
combinatorio :: Int -> Int -> Int
combinatorio n k = (fact n) `div` ((fact k) * fact (n-k))

combinatorio' :: Int -> Int -> Int
combinatorio' n 0 = 1
combinatorio' n k | n == k = 1
                  |otherwise = (combinatorio' (n-1) k) + (combinatorio' (n-1) (k-1))

--b--
agregarElAdelante :: Int -> Set (Set Int) -> Set (Set Int)
agregarElAdelante c [] = []
agregarElAdelante c (h:t) = agregar (c:h) (agregarElAdelante c t)

agregarElementos :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarElementos [] _ = []
agregarElementos (h:t) c = union (agregarElAdelante h c) (agregarElementos t c)

variaciones :: Set Int -> Int -> Set (Set Int)
variaciones c 0 = [[]]
variaciones c k = agregarElementos c (variaciones c (k-1))

--c--
insertarEn :: Set Int -> Int -> Int -> Set Int
insertarEn c n i | i == 1 = n:c
                 | otherwise = (head c):(insertarEn (tail c) n (i-1))

insertarEnCadaPos :: Set Int -> Int -> Int -> Set (Set Int)
insertarEnCadaPos c n 1 = agregar (insertarEn c n 1) []
insertarEnCadaPos c n i = agregar (insertarEn c n i) (insertarEnCadaPos c n (i-1))

insertarEnCadaPosDeTodasLasListas :: Set (Set Int) -> Int -> Set (Set Int)
insertarEnCadaPosDeTodasLasListas [] _ = []
insertarEnCadaPosDeTodasLasListas (h:t) n = (insertarEnCadaPos h n (length h + 1)) `union`
                                            (insertarEnCadaPosDeTodasLasListas t n)

permutaciones :: Set Int -> Set (Set Int)
permutaciones [] = [[]]
permutaciones (h:t) = insertarEnCadaPosDeTodasLasListas (permutaciones t) h

--1--
listade1hastaN :: Int -> Set Int
listade1hastaN 1 = [1]
listade1hastaN n = agregar n (listade1hastaN (n-1))

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas n k = variaciones (listade1hastaN k) n

--2--
pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (h:t) | x == h = True
                  | otherwise = pertenece x t

quitarSin1 :: Set (Set Int) -> Set (Set Int)
quitarSin1 [] = []
quitarSin1 (h:t) | pertenece 1 h = h:(quitarSin1 t)
                 | otherwise = quitarSin1 t

primeraNuncaVacia :: Int -> Int -> Set (Set Int)
primeraNuncaVacia n k = quitarSin1 (bolitasEnCajas n k)

--3--
estaOrdenado :: Set Int -> Bool
estaOrdenado (h:t) | t == [] = True
                   | otherwise = h < (head t) && estaOrdenado t

quitarDesordenados :: Set (Set Int) -> Set (Set Int)
quitarDesordenados [] = []
quitarDesordenados (h:t) | estaOrdenado h = h:(quitarDesordenados t)
                         | otherwise = quitarDesordenados t

listasOrdenadas :: Int -> Int -> Set (Set Int)
listasOrdenadas n k = quitarDesordenados (bolitasEnCajas n k)

--4--
--sucesionesAB :: Int -> Int -> [Char]
--sucesionesAB a b = variaciones

--6--
agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos _ [] = []
agregarATodos x (h:t) = agregar (agregar x h) (agregarATodos x t)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (h:t) = union (partes t) (agregarATodos h (partes t))

quitarDistintoLargo :: Set (Set Int) -> Int -> Set (Set Int)
quitarDistintoLargo [] _ = []
quitarDistintoLargo (h:t) l | length h == l = h:(quitarDistintoLargo t l)
                            | otherwise = quitarDistintoLargo t l

subconjuntos :: Set Int -> Int -> Set (Set Int)
subconjuntos c l = quitarDistintoLargo (partes c) l