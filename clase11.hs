type Complejo = (Float, Float)

re :: Complejo -> Float
re (a,_) = a

im :: Complejo -> Float
im (_,a) = a

conjugado :: Complejo -> Complejo
conjugado (a,b) = (a,-b)

inverso :: Complejo -> Complejo
inverso(a,b) = (a/mod,(-b)/mod)
   where mod = a**2+b**2

--1.4--
suma :: Complejo -> Complejo -> Complejo
suma (a,b) (c,d) = (a+c,b+d)

--1.5--
producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = (a*c-b*d,a*d+b*c)

--1.7--
cociente :: Complejo -> Complejo -> Complejo
cociente (a,b) (c,d) =  ((a*c+b*d)/mod, (b*c-a*d)/mod)
   where mod = c**2+d**2

--1.8--
potencia :: Complejo -> Int -> Complejo
potencia _ p | p <= 0 = undefined
potencia z 1 = z
potencia z p = producto z (potencia z (p-1))

--1.9--
solucionesCuadratica :: Float -> Float -> Float -> (Complejo,Complejo)
solucionesCuadratica a b c | dis >= 0 = ( ( (-b+sqrt(dis))/(2*a), 0                ), ( (-b-sqrt(dis))/(2*a), 0 )                 )
                           | dis < 0  = ( ( -b/(2*a)            , sqrt(-dis)/(2*a) ), ( -b/(2*a)            , -sqrt(-dis)/(2*a) ) )
                 where dis = b**2 - 4*a*c   -- -b/2a +- sqrt(dis)/2a


modulo :: Complejo -> Float
modulo (a,b) = sqrt(a**2+b**2)

argumento :: Complejo -> Float
argumento (a,b) = atan(b/a)

raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada (0,0) = ((0,0),(0,0))
raizCuadrada z = (pasarACartesianas (sqrt(modulo z)) tita, pasarACartesianas (sqrt(modulo z)) (tita+pi))
 where tita = (argumento z)/2

--2.3--
pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r tita | r < 0 = pasarACartesianas (-r) (tita+pi)
                         | tita < 0 = pasarACartesianas r (tita+2*pi)
                         | tita >= 2*pi = pasarACartesianas r (tita-2*pi)
                         | otherwise = (r*cos(tita),r*sin(tita))

--2.5--
resta  :: Complejo -> Complejo -> Complejo
resta (a,b) (c,d) = (a-c,b-d)

solucionesCuadraticaCoefComplejos :: Complejo -> Complejo -> Complejo -> (Complejo,Complejo)
solucionesCuadraticaCoefComplejos a (br,bi) c = ( ((-br,-bi) `suma` (raizCuadrada (dis))) `cociente` 2a, ((-br,-bi) `resta`  (raizCuadrada (dis))) `cociente` 2a )
                                    where dis = (potenica (br,bi) 2) `resta` ((4,0) `producto` a `producto` c)
                                           2a = producto (2,0) a
-- (-b + sqrt (b^2 - 4ac)) /2a
-- (-b - sqrt (b^2 - 4ac)) /2a


raicesNEsimasDesde :: Float -> Float -> [Complejo]
raicesNEsimasDesde k n | k >= n = []
                       | otherwise = (kesimaRaiz):(raicesNEsimasDesde (k+1) n)
                  where kesimaRaiz = (cos(2*k*pi/n), sin (2*k*pi/n))

raicesNEsimas :: Float -> [Complejo]
raicesNEsimas n = raicesNEsimasDesde 0 n

--3.2--
potenciasRaicesNEsima' :: Float -> Float -> Complejo
potenciasRaicesNEsima' k n = (cos(2*k*pi/n), sin (2*k*pi/n))

elevarALaN :: [Complejo] -> Int -> [Complejo]
elevarALaN [] _ = []
elevarALaN (h:t) n = (potencia h n):(elevarALaN t n)

potenciasRaicesNEsima :: Float -> Int -> [Complejo]
potenciasRaicesNEsima k n = elevarALaN (raicesNEsimas k) n  --?????