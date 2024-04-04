valorAbsoluto :: Float -> Float
valorAbsoluto a | a >= 0 = a
                | otherwise = -a

bisiesto :: Int -> Bool
bisiesto a | (mod a 400 == 0) = True
           | (mod a 100 == 0) = False
           | (mod a 4 == 0) = True
           | otherwise = False

factorial :: (Integral a) => a -> a
factorial a | (a == 1) = 1
            | otherwise = a * factorial( a - 1)


cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos a = cantDivisoresPrimosHasta 2 a


cantDivisoresPrimosHasta :: Int -> Int -> Int
cantDivisoresPrimosHasta contador num | (contador > num) = 0
                                      | (esPrimo contador && (mod num contador == 0)) = 1 + cantDivisoresPrimosHasta (contador + 1) num
                                      | otherwise = cantDivisoresPrimosHasta (contador + 1) num

esPrimo :: Int -> Bool
esPrimo a = esPrimoAux 1 a

esPrimoAux:: Int -> Int -> Bool
esPrimoAux a b | (a == b) = True
               | (a == 1) = esPrimoAux (a+1) b
               | (mod b a == 0) = False
               | otherwise = esPrimoAux (a+1) b