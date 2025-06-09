menorDivisor :: Integer -> Integer -> Integer
menorDivisor 1 _ = 1
menorDivisor n k | n `mod` k == 0 = k
                 | otherwise = menorDivisor n (k+1)

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n 2

--1--

--Daniel: esta funcion quizas se deberia llamar mayorDivisorComun.    Fixed

mayorDivisorComunHasta :: Integer -> Integer -> Integer -> Integer
mayorDivisorComunHasta a b m | a `mod` m == 0 && b `mod` m == 0 = m
                             | otherwise = mayorDivisorComunHasta a b (m-1)

--Daniel: en esta funcion de abajo, yo haria al reves: si a > b, entonces buscaria el 
--mayor divisor comun hasta b (en vez de hasta a), porque el mayor divisor comun es a lo sumo
--igual al menor de los dos numeros. Igual funciona correctamente asi como esta.                Fixed


sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b | a < 1 || b < 1 = undefined            --sólo definido para números Naturales
                | a > b = 1 == mayorDivisorComunHasta a b b
                | otherwise = 1 == mayorDivisorComunHasta a b a

--2--
es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo n | n >= 3 && (not (esPrimo n))= (2^(n-1)-1) `mod` n == 0 --las condiciones son que cumplan la ecuación,
                 | otherwise = False                                     --que no sean primos y que sean mayor o igual a 3

--3--
es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo n | n >= 3 && (not (esPrimo n))= (3^(n-1)-1) `mod` n == 0
                 | otherwise = False

--Daniel: Tu solucion es correcta, pero me parece muy rebuscada. Te propongo que pienses si lo podes hacer 
--sin usar un contador, de manera directa, usando tu misma idea. Que relacion hay entre cantidad3Pseudoprimos m 
--y cantidad3Pseudoprimos (m-1)? eso no te da ya una formula recursiva?                                          

--Mariano: No sabria como hacerlo de otra forma :/ "cantidad" no involucra contar? como puedo contar sin un contador?

contadorDe3PsP :: Integer -> Integer -> Integer --(3PsP por 3-pseudoprimos)
contadorDe3PsP 1 c = c
contadorDe3PsP i c | es3Pseudoprimo i = contadorDe3PsP (i-1) (c+1) -- si es 3-pseudoprimo, lo cuenta
                   | otherwise = contadorDe3PsP (i-1) c            -- si no es 3-pseudoprimo, no lo cuenta

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos m = contadorDe3PsP m 0

--Mariano: podria ser algo así?:
contarSinContador :: Integer -> Integer  -- para cantidad3Pseudoprimos
contarSinContador 1 = 0
contarSinContador m | es3Pseudoprimo m = 1 + contarSinContador (m-1)
                    | otherwise = contarSinContador (m-1)

--4--

--Daniel: Veo que te gustan los contadores. Pero hacen mucho mas complicadas las cosas de lo que son realmente!
--Nuevamente, te propongo que pienses alguna solucion mas directa. Que relacion hay entre kesimo2y3pseudoprimo con k y con (k-1)?   Fixed?

contadorDe2y3PsP :: Integer -> Integer -> Integer -> Integer
contadorDe2y3PsP k i c | k == c = i
                       | es3Pseudoprimo i && es2Pseudoprimo i = contadorDe2y3PsP k (i+1) (c+1)
                       | otherwise = contadorDe2y3PsP k (i+1) c

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo k = contadorDe2y3PsP k 3 1

--Mariano: No se si es esto a lo que te referias

noContador:: Integer -> Integer -> Integer
noContador 0 c = c
noContador k c | es3Pseudoprimo c && es2Pseudoprimo c = noContador (k-1) (c+1)
               | otherwise = noContador k (c+1)

seguimosSinContadores :: Integer -> Integer
seguimosSinContadores k | k < 1 = undefined
                        | otherwise = noContador k 3

--Mariano: lo probe un par de veces en la consola y como decimos en el técnico, si no está roto no lo arregles

--5--
--Daniel: Aca falta chequear que n sea compuesto, lo cual deberia estar en la funcion que se llama "esAPseudoprimo", +
--pero veo que chequeas esa condicion en la funcion esCarmichael.  

-- Mariano: Si, me pareció mejor probarlo 1 vez en esCarmichael que todas las veces que llame a la funcion esAPseudoprimo

esAPseudoprimo :: Integer -> Integer -> Bool
esAPseudoprimo a n = (a^(n-1)-1) `mod` n == 0

--Daniel: =)

queComplicado :: Integer -> Integer -> Bool
queComplicado n a | a == n = True                                 --si a logra llegar al mismo valor que n quiere decir que pasó todas las pruebas
                  | not (sonCoprimos a n) = queComplicado n (a+1) --si ese a no es coprimo, lo descarto y sigo
                  | esAPseudoprimo a n = queComplicado n (a+1)
                  | otherwise = False                             --si a comprimo y n no es a-pseudoprimo, entonces n no es Carmichael

esCarmichael :: Integer -> Bool
esCarmichael n | esPrimo n || n <= 1 = False   -- n no tiene que ser primo ni menor igual a 1
               | otherwise = queComplicado n 1 -- tal vez no se vea complicado pero pensarlo lo fue