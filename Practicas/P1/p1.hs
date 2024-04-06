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


-- Ejercicio 2

{-
Basicamente me sirven para usar una funcion currificada 
pasandole los parametros como tupla y que esto se encargue
de ejecutar la funcion como corresponde. Lo mismo con las
funciones sin currificar.
-}

curryMio :: ((a,b) -> c) -> (a -> b -> c)
curryMio f = \x y -> f (x, y)

unCurryMio :: (a -> b -> c) -> ((a,b) -> c)
unCurryMio f = \(x,y) -> f x y


-- Ejercicio 3

{-
Redefinir usando foldr las funciones sum, elem, (++), filter y map.
-}

sumFoldR :: (Num a) => [a] -> a
sumFoldR = foldr (\x acc -> x + acc) 0

elemFoldr :: (Eq a) => a -> [a] -> Bool    
elemFoldr item = foldr (\x acc -> x == item || acc) False

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\x acc -> if f x then x : acc else acc) []

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) []

-- Ejercicio 4

{-
Definir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutacio-
nes. Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop.
-}

import Data.List (insert)
import Data.Ord (comparing)
import Data.List (maximumBy)

-- Definición de la función listaMasLarga
listaMasLarga :: [[a]] -> [a]
listaMasLarga = maximumBy (comparing length)

--insert posicion elemento lista

permutaciones :: [a] -> [[a]]
permutaciones lista = permutacionesAux contador lista

permutacionesAux :: a -> [a] -> [[a]]
permutacionesAux a lista = take a 


