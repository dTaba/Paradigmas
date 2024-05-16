sumFoldr :: (Num a) => [a] -> a
sumFoldr = foldr (+) 0

elemFoldr :: (Eq a) => a -> [a] -> Bool
elemFoldr valor = foldr (\x acc -> x == valor || acc) False

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\x acc -> if f x then x : acc else acc) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x acc -> if f x acc then x else acc)

sumasParciales :: (Num a) => [a] -> [a]
sumasParciales lista = reverse(foldl (\acc x -> if null acc then x : acc else (x + head acc): acc) [] lista)

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (\x acc -> x - acc) 0

sumaAltL :: Num a => [a] -> a
sumaAltL = foldl (\acc x -> x - acc) 0