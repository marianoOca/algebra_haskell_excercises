esPar :: Int -> Bool
esPar n = mod n 2 == 0

estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | x <= 3 && y <= 3 = True
                      | 3 < x && x <= 7 && 3 < y && y <= 7 = True
                      | x > 7 && y > 7 = True
                      | otherwise = False

prodInt :: (Float , Float ) -> (Float , Float ) -> (Float , Float )
prodInt v w = ((fst v)*(fst w) , (snd v)*(snd w))

todoMenor :: (Float , Float ) -> (Float , Float ) -> Bool
todoMenor v w = (fst v) < (snd v) && (fst w) < (snd w)

distanciaPuntos :: (Float , Float ) -> (Float , Float ) -> Float
distanciaPuntos v w = sqrt(((fst v) - (fst w))^2 + ((snd v) - (snd w))^2)

sumaTerna :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
sumaTerna (vx, vy, vz) (wx, wy, wz) = (vx + wx, vy + wy, vz + wz)

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (vx, vy, vz) | esPar vx = 1
                            | esPar vy = 2
                            | esPar vz = 3
                            | otherwise = 4

crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)

invertir:: (a,b) -> (b,a)
invertir v = ((snd v),(fst v))

algunoEs0Ternas :: (Int, Int, Int) -> Bool
algunoEs0Ternas (0, _, _) = True
algunoEs0Ternas (_, 0, _) = True
algunoEs0Ternas (_, _, 0) = True
algunoEs0Ternas _ = Falseyo