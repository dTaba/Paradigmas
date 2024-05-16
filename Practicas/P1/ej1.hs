-- Ej 1)

{-
Agregar los tipos a cada
una de las funciones e indicar si estan
currificadas o no
-}

{- 
max2
Es no currificada ya que recibe una tupla,
en vez de x,y de manera separada
-}

max2 :: (Float, Float) -> Float
max2 (x, y)| x >= y = x
           | otherwise = y

max2Curry :: Float -> Float -> Float
max2Curry x y | x >= y = x
         | otherwise = y

{- 
normaVectorial
Es no currificada ya que recibe una tupla,
en vez de x,y de manera separada
-}

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

normaVectorialCurry :: Float -> Float -> Float
normaVectorialCurry x y = sqrt (x^2 + y^2)


{- 
subtract
Es currificada ya que recibe dos
parametros por separado
-}

subtractFlip :: Float -> Float -> Float
subtractFlip = flip (-)


{- 
predecesor
Es currificada ya que recibe un
parametro que no es una tupla
-}

predecesor :: Float -> Float
predecesor = subtract 1


{- 
evaluarEnCero
Es currificada ya que recibe una funcion
y devuelve un float
-}

evaluarEnCero :: (Float -> Float) -> Float
evaluarEnCero = \f -> f 0


{- 
dosVeces
Es currificada ya que recibe una funcion y devuelve
otra
-}

dosVeces :: (Float -> Float) -> (Float -> Float)
dosVeces = \f -> f . f


{- 
flipAll

map = (a -> b) -> [a] -> [b]
flip = (a -> b -> c) -> b -> a -> c

a -> b -> c = a'
b -> a -> c = b'

map = (a' -> b') -> [a'] -> [b']
-}

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

{- 
flipRaro

flip = (a -> b -> c) -> b -> a -> c

flip2 = (a -> b -> c) -> b -> a -> c 
      = (a' -> b' -> c')

a' = a -> b -> c
b' = b 
c' = a -> c 
-}

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip