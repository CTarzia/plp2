recr' :: b -> (a -> [a] -> b -> b) -> [a] -> b
recr' z _ [] = z
recr' z f (x:xs) = f x xs (recr' z f xs)

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

{-permutaciones :: [a] -> [[a]] --ayuda
permutaciones  = foldr (\x rec -> (intercalar x rec) ++ rec) []
-}
put :: Int ->  a -> [a] -> [a]
put n elem xs = let (ys,zs) = splitAt n xs 
                in ys ++ [elem] ++ zs

--intercalar :: a -> [[a]] -> [[a]]
--intercalar i = foldr (\x rec -> ++ rec) []



--e11
{-partes :: [a] -> [[a]]
partes = -}

--e12
recr :: (a->[a]->b->b)->b->[a]->b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna elem = recr (\x xs rec -> if (elem == x) then rec else x:xs) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem = recr (\x xs rec -> if ((x <= elem) && (elem < (head xs))) then x:elem:xs else x:rec) []

--ej 14
mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = foldr (\elem rec -> (g elem):rec) []
             where g = uncurry f

listaPares :: [(Int, Int)]
listaPares = [(x,x+1) | x<- [1..]]

armarPares :: [a] -> [b] -> [(a,b)]
armarPares xs ys = foldr f (const []) xs ys
           where f x g = recr (\y ys rec -> (x,y) : (g ys)) []

mapDoble :: (a -> b -> c) -> [a] -> [b] ->[c]
mapDoble f xs ys =  foldr (\elem rec -> (g elem):rec) [] (armarPares xs ys)
                where g = uncurry f

--ej 16
generate :: ([a]->Bool)->([a]->a)->[a]
generate stop next = generateFrom stop next []

generateFrom :: ([a]->Bool)->([a]->a)->[a]->[a]
generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])

generateBase :: ([a]->Bool) -> a -> (a -> a) -> [a]
generateBase stop first next = generate stop baseNext
           where baseNext [] = first
                 baseNext xs = next (last xs)

factoriales :: Int -> [Int]
factoriales n = generate (\xs -> length xs == (n+1)) (\xs -> if (length xs) < 2 then 1 else last xs * (length xs))

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase (\xs -> length xs == n) x (\x -> f x) 

{-generateFrom' :: ([a]->Bool)->([a]->a)->[a]->[a] --ayuda?
generateFrom' stop next xs =  takeWhile (\xs -> stop xs) (iterate next (if xs == [])) []
-}

--e17
foldNat :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldNat = foldr

--no entendi el 2

--ej 19
--e 19
type Conj a = (a->Bool)

conj1 = (\x -> x == 1 || x == 2)
conj2 = (\x -> x == 2 || x == 3)

vacio :: Conj a
vacio = (\x -> False)

agregar :: Eq a => a -> Conj a -> Conj a
agregar x c = (\y -> (c y) ||  (x == y))

interseccion :: Conj a -> Conj a -> Conj a
interseccion c1 c2 = (\x -> (c1 x) && (c2 x))

union :: Conj a -> Conj a -> Conj a
union c1 c2 = (\x -> (c1 x) || (c2 x))

{-conjLoco :: Conj (Int -> Int)
conjLoco =  agregar conjLoco (agregar (\x-> True) vacio)-}

--singleton :: Eq a => a -> Conj a

--e21
data AHD tInterno tHoja = Hoja tHoja
                        | Rama tInterno (AHD tInterno tHoja)
                        | Bin (AHD tInterno tHoja) tInterno (AHD tInterno tHoja)
                        deriving Show

hoja = Hoja 1
rama = Rama 2 (hoja)
arbol = Bin rama 3 rama

foldAHD :: (tHoja -> b) ->
           (tInterno -> b -> b) ->
           (b -> tInterno -> b -> b) ->
           AHD tInterno tHoja -> b
foldAHD cH cR cB ahd = case ahd of
                       (Hoja h) -> cH h
                       (Rama i a) -> cR i (rec a)
                       (Bin a1 i a2) -> cB (rec a1) i (rec a2)
                       where rec = foldAHD cH cR cB

mapAHD :: (a->b) -> (c->d) -> AHD a c -> AHD b d
mapAHD fI fH ahd = case ahd of 
                   (Hoja h) -> Hoja (fH h)
                   (Rama i a) -> Rama (fI i) (rec a)
                   (Bin a1 i a2) -> Bin (rec a1) (fI i) (rec a2)
                   where rec = mapAHD fI fH

--p1e23
data RoseTree a = Rose a [RoseTree a]

recRT :: b -> (Rose a -> RoseTree a -> b -> b) -> RoseTree a -> b -- ayuda
recRT z f (Rose r []) = f r z

