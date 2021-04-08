prodInt :: (Float, Float)-> (Float, Float) -> Float
prodInt (x, y) (w, z) = x*w + y*z

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x, y) (u,v) | x < v = True
                      | otherwise = False

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x,y) (s,t) = sqrt((x-s)**2 + (y-t)**2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x,y,z) | mod x 2 == 0 = 1
                       | mod y 2 == 0 = 2
                       | mod z 2 == 0 = 3
                       | otherwise = 4

--even es la funcion que me dice si un numero es ''par''. Ejemplo, even 2 devuelve True.--
posic :: (Int,Int,Int)->Int 
posic (x,y,z) | even x = 1 
              | even y = 2
              | even z = 2
              |otherwise = 4

crearPar :: p1 -> p2 -> (p1, p2)
crearPar x y = (x,y)

inventir :: (p1, p2) -> (p2, p1)
inventir (a,b) = (b,a)
