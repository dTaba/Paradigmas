elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs 
                                       then [x]
                                       else x : elementosEnPosicionesPares (tail xs)