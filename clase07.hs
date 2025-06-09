type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (h:t) | x == h = True
                  | otherwise = pertenece x t

agregar :: Int -> Set Int -> Set Int
agregar x c | pertenece x c = c
            | otherwise = x:c

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (h:t) c = pertenece h c && incluido t c

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = incluido c1 c2 && incluido c2 c1

--1--
union :: Set Int -> Set Int -> Set Int
union [] c = c
union (h:t) c | pertenece h c = union t c
              | otherwise = union t (h:c)

--2--
intAux :: Set Int -> Set Int -> Set Int -> Set Int
intAux [] _ s = s
intAux (h:t) c s | pertenece h c = intAux t c (h:s)
                 | otherwise = intAux t c s

interseccion :: Set Int -> Set Int -> Set Int
interseccion c1 c2 = intAux c1 c2 vacio

--3--
difAux :: Set Int -> Set Int -> Set Int -> Set Int
difAux [] _ s = s
difAux (h:t) c s | not (pertenece h c) = difAux t c (h:s)
                 | otherwise = difAux t c s

diferencia :: Set Int -> Set Int -> Set Int
diferencia c1 c2 = union (difAux c1 c2 vacio) (difAux c2 c1 vacio)

--4--
diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica c1 c2 = diferencia (union c1 c2) (interseccion c1 c2)

--5--
perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC _ [] = False
perteneceC x (h:t) = x == h || perteneceC x t

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC x c | perteneceC x c = c
             | otherwise = x:c

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] c = c
unionC (h:t) c | perteneceC h c = unionC t c
               | otherwise = unionC t (h:c)

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos _ [] = []
agregarATodos x (h:t) = agregarC (agregar x h) (agregarATodos x t)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (h:t) = unionC (partes t) (agregarATodos h (partes t))

--6--
parN :: Set Int -> Set (Set Int)
parN (h:[-1]) = [[]]
parN (h:[n]) = unionC (parN (n:[n-1])) (agregarATodos h (parN (n:[(n-1)])))

partesN :: Int -> Set (Set Int)
partesN n = parN (n:[n-1])

--7--
unirTuplas :: Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
unirTuplas c [] = c
unirTuplas c (h:t) = h:(unirTuplas c t)

tuplanizador :: Set Int -> Int -> Set (Int,Int)
tuplanizador [] _ = []
tuplanizador (h:t) x = (h,x):(tuplanizador t x)

productoCartesioano :: Set Int -> Set Int -> Set (Int,Int)
productoCartesioano _ [] = []
productoCartesioano c (h:t) = unirTuplas (tuplanizador (c) h) (productoCartesioano c t)