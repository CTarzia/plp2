shuffle :: [Int] -> [a] -> [a]
shuffle li xs = map (xs !!) li

deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN long ls = filter (tieneEsaLongitud long) ls

tieneEsaLongitud :: Int -> [a] -> Bool
tieneEsaLongitud long = (\l -> (length l) == long)

soloPuntosFijosEnN :: Int -> [Int->Int] -> [Int->Int]
soloPuntosFijosEnN n lf = filter (evaluaN n) lf

evaluaN :: Int -> (Int->Int) -> Bool
evaluaN n = (\f -> (f n) == n)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort (filter (< x) xs)) ++ [x] ++ (quickSort (filter (>= x) xs))

reverseAnidado :: [[Char]] -> [[Char]]
reverseAnidado ls = reverse (map reverse ls)

paresCuadrados :: [Int] -> [Int]
paresCuadrados l = map cuadrado (filter esPar l)
                   where cuadrado = (\x -> x*x)
                         esPar = (\x -> ((x `mod` 2) == 0))
