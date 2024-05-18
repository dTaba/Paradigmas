recr :: (a -> [a] -> [a] -> [a]) -> [a] -> [a] -> [a]
recr _ acc [] = acc
recr f acc (x:xs) = f x xs (recr f acc xs)

sacarUna :: (Eq a) => a -> [a] -> [a]
sacarUna elem = recr (\x xs acc -> if x == elem then xs else x : acc) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem =  recr (\x xs recc -> if (elem > x && elem < head(xs)) then x: elem : xs else x : recc) []