sumaL :: [Int] -> Int
sumaL [] = 0
sumaL (x:xs) = x + sumaL xs

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ concat xs