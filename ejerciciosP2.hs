sumaL:: Num a => [a] -> a
sumaL [] = 0
sumaL (x:xs) = x + sumaL xs


concatenacion:: [[a]] -> [a]
concatenacion [] = []
concatenacion (xs:xss) = xs ++ (concatenacion xss)

reverso:: [a] -> [a]
reverso [] = []
reverso (x:xs) = ((reverso xs)++[x])
