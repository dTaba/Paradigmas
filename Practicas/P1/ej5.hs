elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)

{- 

No es recursion estructural.

1. Devuelve un valor fijo en el caso base - OK
2. No hace recursion sobre lal cola! le esta sacando un elemento! - Mal 
3. Usa el valor de la cabeza pero tambien el de la cola! - Mal

-}

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

entrelazarFoldr :: [a] -> [a] -> [a]
entrelazarFoldr = foldr (\x recc -> \ y -> x:head(y):recc(tail y)) id