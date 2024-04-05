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


inverso :: Float -> Maybe Float
inverso a | (a == 0) = Nothing
          | otherwise = Just(1/a) 


aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right True) = 1
aEntero (Right False) = 0


-- 4)a) 

limpiar :: String -> String -> String
limpiar [] (y:ys) = y:ys   
limpiar (x:xs) (y:ys) 
    | elem x ([y] ++ ys) = limpiar xs (limpiarAux x ([y] ++ ys) [])
    | otherwise = y : limpiar xs ys 

limpiarAux :: Char -> String -> String -> String
limpiarAux _ [] res = res
limpiarAux x (y:ys) res
    | y == x    = limpiarAux x ys res
    | otherwise = limpiarAux x ys (res ++ [y])

-- 4)b)

difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio (x:xs) = difPromedioAux ([x] ++ xs) (sumarElementos ([x] ++ xs) / fromIntegral (length ([x] ++ xs))) 

difPromedioAux :: [Float] -> Float -> [Float]
difPromedioAux [] _ = []
difPromedioAux (x:xs) p = x - p : difPromedioAux xs p 

sumarElementos :: [Float] -> Float
sumarElementos [] = 0
sumarElementos (x:xs) = (x + sumarElementos xs) 

-- 4)c)

todosIguales :: [Int] -> Bool
todosIguales (x:xs)
    | xs == [] = True
    | (x == head xs) = todosIguales xs
    | otherwise = False


--5)a)


data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq valor der) = Bin (negacionAB izq) (not valor) (negacionAB der)

