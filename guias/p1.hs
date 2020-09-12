recr :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr z _ [] = z
recr z f (x:xs) = f x xs (recr z f xs)

--ej 4
pitagoricas :: [(Int, Int, Int)]
pitagoricas = [(x,y,z) | z<-[1..], x<-[1..z], y<-[1..z], x^2+y^2==z^2, x<y ]

--ej 5
primos :: [Int]
primos = [x | x <- [1..], esPrimo x]

milPrimos :: [Int]
milPrimos = take 1000 primos

esPrimo :: Int -> Bool
esPrimo x
          | x == 0 = False
          | x == 1 = False
          | x == 2 = True
          | otherwise = esPrimoAux x (x-1)

esPrimoAux :: Int -> Int -> Bool
esPrimoAux x 1 = True
esPrimoAux x y = if (x `esDivididoPor` y) then False else (esPrimoAux x (y-1))
      where esDivididoPor x y = mod x y == 0

-- ej 6
{-partir :: [a] -> [([a],[a])]
partir l = [(l1, l2) | i <- [0..(length l)], l1 <- (take i l), l2 <- (drop i l)] -}

--ej 7
{-listasQueSuman :: Int -> [[Int]]
listasQueSuman 1 = [[1]]
listasQueSuman x = (map agregarUnoALista (listasQueSuman x-1)) ++ (map sumarUnoACadaElem (listasQueSuman x-1))
               where agregarUnoALista l = (1:l)

sumarUnoACadaElem :: [Int] -> [[Int]]
sumarUnoACadaElem l = 
	              let aux l i 
	              in aux l (length l)
-}
--p1e7
listasQueSuman2 :: Int -> [[Int]]
listasQueSuman2 0 = [[]]
listasQueSuman2 n = concat [map (\l -> (n-k):l) (listasQueSuman2 k) | k <- [0..n-1]]

-- p1e8
{-superLista ::[[Int]]
superLista = (agregarACadaUno superLista [1..]) ++ superLista

agregarACadaUno :: [[Int]] -> [Int] -> [[Int]]
agregarACadaUno listaOriginal numsAAgregar = recr listaOriginal (agregarAUno numsAAgregar) listaOriginal

agregarAUno :: [Int] -> [Int] -> [[Int]] 
agregarAUno numsAAgregar l = recr l (\n -> \ns -> l ++ [n]) numsAAgregar-}

--ej 9 --ayuda
type DivideConquer a b = (a -> Bool) --es o no trivial
                       -> (a -> b)  --resuelve caso trivial
                       -> (a -> [a]) -- parte en subproblemas
                       -> ([b] -> b) -- combina resultados
                       -> a -- estructura de entrada
                       -> b -- resultado

dc :: DivideConquer a b
dc trivial solve split combine x
                          | trivial x = solve x
                          | otherwise = combine (map solve (split x))

mergeSort :: Ord a => [a] -> [a]
mergeSort = dc (\l -> (length l) < 2) 
               id 
               (\l -> [take (div (length l) 2) l, drop (div (length l) 2) l]) 
               (\ls -> unirListasOrdenadas [] (tuplify2 ls))
            
tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

unirListasOrdenadas :: Ord a => [a] -> ([a],[a]) -> [a] --ASHUDA
unirListasOrdenadas res (l1,[]) = res ++ l1
unirListasOrdenadas res ([],l2) = res ++ l2
unirListasOrdenadas res ((x:xs),(y:ys)) = if (x < y) then unirListasOrdenadas (res ++ [x]) (xs, (y:ys))
                                                     else unirListasOrdenadas (res ++ [y]) ((x:xs), ys)
{-
map' :: (a->b)->[a]->[b]
map' = dc (\f l -> (length l) < 2)
          (\f l -> if (l == []) then [] else [f (l !! 0)])
          (\f l -> )
-}

--ej 10
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\y rec -> rec || x == y) False

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x rec -> if (f x) then x:rec else rec) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x else y)

sumasParciales::Num a => [a]->[a] --ayuda
sumasParciales l = reverse (foldr (\x rec -> ((if (length rec > 0) then head rec else 0) + x) : rec) [] (reverse l))

sumaAlt :: [Int] -> Int
sumaAlt = foldr (-) 0

sumaAlt2 :: [Int] -> Int
sumaAlt2 l = (if (length l > 0) then head l else 0) + sumaAlt (tail l)

permutaciones :: [a] -> [[a]] --ayuda
permutaciones 

--e11
{-partes :: [a] -> [[a]]
partes = -}

