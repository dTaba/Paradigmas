curryMio :: ((a,b) -> c) -> (a -> b -> c)
curryMio f = \x y -> f(x, y)

unCurryMio :: (a -> b -> c) -> ((a, b) -> c)
unCurryMio f = \(x,y) -> f x y