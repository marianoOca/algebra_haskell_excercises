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

mcd :: Int -> Int -> Int
mcd a 0 = abs a
mcd a b = mcd b (mod a b)

mcm :: Int -> Int -> Int
mcm a b = mod (mcd a b) (a*b)

emcd :: Int -> Int -> (Int,Int,Int)
emcd a 0 | a > 0 = (a, 1, 0)
         | otherwise = (abs a, -1, 0)
emcd a b = (d, t', s' - t'*q)
 where q = div a b
       r = mod a b
       (d, s', t') = emcd b r

-- 1 ---------------------
ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente (a, b, m) | mod b d /= 0  = undefined
                        | otherwise = (div a d, div b d, div m d)
 where d = mcd a m 

solucionEcConPropAdic :: (Int, Int, Int) -> (Int, Int)
solucionEcConPropAdic (a, b, m) = (mod (s*b) m, m)
 where (d, s, t) = emcd a m 

solucionEc :: (Int, Int, Int) -> (Int, Int)
solucionEc e = solucionEcConPropAdic (ecEquivalente e)

-- 2 --------------------
sistemaSimplifEquiv :: [(Int, Int, Int)] -> [(Int, Int)]
sistemaSimplifEquiv [] = []
sistemaSimplifEquiv (e:es) = (solucionEc e):(sistemaSimplifEquiv es)

-- 3 --------------------
modulos :: [(Int, Int)] -> [Int]
modulos [] = []
modulos ((r, m):es) = m:(modulos es)

mayorModulo :: [(Int, Int)] -> Int
mayorModulo sist = maximum (modulos sist)

cotaParaPrimoMaloDesde :: [(Int, Int)] -> Int -> Int
cotaParaPrimoMaloDesde sist n | nesimoPrimo (n+1) > (mayorModulo sist) = n
                              | otherwise = cotaParaPrimoMaloDesde sist (n+1)

cotaParaPrimoMalo :: [(Int, Int)] -> Int
cotaParaPrimoMalo sist = cotaParaPrimoMaloDesde sist 1

cantidadMultiplos :: [Int] -> Int -> Int
cantidadMultiplos [] _ = 0
cantidadMultiplos (m:ms) n | mod m (nesimoPrimo n) == 0 = 1 + cantidadMultiplos ms n
                           | otherwise = cantidadMultiplos ms n 

esPrimoMalo :: [(Int, Int)] -> Int -> Bool
esPrimoMalo sist n = cantidadMultiplos (modulos sist) n >= 2


todosLosPrimosMalosHasta :: [(Int, Int)] -> Int -> [Int]
todosLosPrimosMalosHasta _ 0 = []
todosLosPrimosMalosHasta sist n | esPrimoMalo sist n = (nesimoPrimo n):(todosLosPrimosMalosHasta sist (n-1))
                                | otherwise = todosLosPrimosMalosHasta sist (n-1)

todosLosPrimosMalos :: [(Int, Int)] -> [Int]
todosLosPrimosMalos [] = []
todosLosPrimosMalos sist = todosLosPrimosMalosHasta sist (cotaParaPrimoMalo sist)

-- 4 --------------------
solucDosEcPotenciasPrimoOrd :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2) | mod (r2-r1) m1 == 0 = (r2, m2)
                                              | otherwise = undefined

solucDosEcPotenciasPrimo :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimo (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimoOrd (r1, m1) (r2, m2)
                                           | otherwise = solucDosEcPotenciasPrimoOrd (r2, m2) (r1, m1)

solucSistemaPotenciasPrimo :: [(Int, Int)] -> (Int, Int)
solucSistemaPotenciasPrimo [e] = e
solucSistemaPotenciasPrimo (e1:e2:es) = solucSistemaPotenciasPrimo ((solucDosEcPotenciasPrimo e1 e2):es)

-- 5 --------------------
quPoLoDi :: Int -> Int -> Int -> Int
quPoLoDi m n p | mod m (n^p) == 0 = 1 + quPoLoDi m n (p+1)
               | otherwise = -1

quePotenciaLoDivide :: Int -> Int -> Int
quePotenciaLoDivide m p = quPoLoDi m p 0


desdoblarSistemaEnFcionPrimo :: [(Int, Int)] -> Int -> ([(Int, Int)], [(Int, Int)])
desdoblarSistemaEnFcionPrimo [] _ = ([], [])
desdoblarSistemaEnFcionPrimo ((r, m):es) p | k == 0 = (pri, (r, m):seg)
                                           | m == p^k = ((r, m):pri, seg)
                                           | otherwise = ((mod r (p^k), p^k):pri, (mod r (div m (p^k)), div m (p^k)):seg)
 where (pri, seg) = desdoblarSistemaEnFcionPrimo es p 
       k = quePotenciaLoDivide m p

sistemaEquivSinPrimosMalosAux :: [(Int, Int)] -> [Int] -> [(Int, Int)]
sistemaEquivSinPrimosMalosAux sist [] = sist
sistemaEquivSinPrimosMalosAux sist (p:ps) = (solucSistemaPotenciasPrimo pri):(sistemaEquivSinPrimosMalosAux seg ps)  
 where (pri, seg) = desdoblarSistemaEnFcionPrimo sist p

sistemaEquivSinPrimosMalos :: [(Int, Int)] -> [(Int, Int)]
sistemaEquivSinPrimosMalos sist = sistemaEquivSinPrimosMalosAux sist (todosLosPrimosMalos sist)

-- 6 -------------------
solucSistemaModCoprimos :: [(Int, Int)] -> (Int, Int)
solucSistemaModCoprimos [e] = e
solucSistemaModCoprimos ((r1, m1):(r2, m2):es) = solucSistemaModCoprimos ((r, m1*m2):es)
 where (d, s, t) = emcd m1 m2
       r = mod (r1*t*m2 + r2*s*m1) (m1*m2)

solucSistema :: [(Int, Int, Int)] -> (Int, Int)
solucSistema sist = solucSistemaModCoprimos ( sistemaEquivSinPrimosMalos ( sistemaSimplifEquiv sist) )

--Ejs
--1--
ecEquivalente' :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente' (a, b, m) | mod b d /= 0  = (0,0,0)
                         | otherwise = (div a d, div b d, div m d)
 where d = mcd a m 

esteTieneSol :: (Int,  Int, Int) -> Bool
esteTieneSol e = not (ecEquivalente' e == (0,0,0))

cadaEcTieneSoluc :: [(Int, Int, Int)] -> [Bool]
cadaEcTieneSoluc [] = []
cadaEcTieneSoluc (e:es) = (esteTieneSol e):(cadaEcTieneSoluc es)

--2--
solucDosEcPotenciasPrimoOrd' :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimoOrd' (r1, m1) (r2, m2) | mod (r2-r1) m1 == 0 = (r2, m2)
                                               | otherwise = (0,0)

solucDosEcPotenciasPrimo' :: (Int, Int) -> (Int, Int) -> (Int, Int)
solucDosEcPotenciasPrimo' (r1, m1) (r2, m2) | m1 <= m2 = solucDosEcPotenciasPrimoOrd' (r1, m1) (r2, m2)
                                            | otherwise = solucDosEcPotenciasPrimoOrd' (r2, m2) (r1, m1)

solucSistemaPotenciasPrimo' :: [(Int, Int)] -> (Int, Int)
solucSistemaPotenciasPrimo' [e] = e
solucSistemaPotenciasPrimo' (e1:e2:es) = solucSistemaPotenciasPrimo' ((solucDosEcPotenciasPrimo' e1 e2):es)

tieneSolucionSimplif :: [(Int, Int)] -> Bool
tieneSolucionSimplif sistemas = not (solucSistemaPotenciasPrimo' sistemas == (0,0))
--3--
--tieneSolucion :: [(Int, Int, Int)] -> Bool

--4--
dirichlet :: Int -> Int -> Int
dirichlet r m = dirichletDesde r m r

dirichletDesde :: Int -> Int -> Int -> Int
dirichletDesde r m k | esPrimo k = k
                     | otherwise = dirichletDesde r m (k+m)