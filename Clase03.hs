multi3 :: Int -> Bool
multi3 1 = False
multi3 2 = False
multi3 3 = True
multi3 n = multi3 (n-3)

sumaN :: Int -> Int
sumaN 1 = 1
sumaN n = n + sumaN (n-1)

sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares n = (2*n-1) + sumaImpares (n-1)

medioFact :: Int -> Int
medioFact 1 = 1
medioFact 2 = 2
medioFact n | mod n 2 == 0 = n*medioFact(n-2)
            | mod n 2 /= 0 = n*medioFact(n-2)
            |otherwise = 0

sumaDigitos :: Int -> Int
sumaDigitos n | n > 0 && n < 10 = n
              | n >= 10 = mod n 10 + sumaDigitos (div (n-1) 10)
              | otherwise = 0

restaDigitos :: Int -> Int
restaDigitos n = (-1)*sumaDigitos n

-- dado un numero n, sumos los digitos, variando su signo. El primero es positivo, el segundo negativo y asi --
sumados :: Int -> Int
sumados n | n > 0 && n < 10 = n
          | n >= 10 = mod n 10 - sumados (div (n-1) 10)
          | otherwise = 0

digitosIguales :: Int -> Bool
digitosIguales n | (sumados n == (mod n 10) || sumados n == 0) = True
                 | otherwise = False

digitosIguales2 :: Int  -> Bool
digitosIguales2 n | n < 10 = True
                  | n < 100 = (mod n 10) == (div (mod n 100) 10)
                  | otherwise = ((mod n 10) == (div (mod n 100) 10)) && digitosIguales2 (div n 10)
