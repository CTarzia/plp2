--conjuntos como funciones
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

--Definir un conjunto de funciones que contenga infinitos elementos, y dar su tipo.
{-conjLoco :: Conj (Int -> Int)
conjLoco = (\x -> ) 


singleton :: Eq a => a -> Conj a
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

listasPositivas:: [[Int]]
listasPositivas = concat [listasQueSuman2 n | n <- [1..]]

--p1e10
elem2 :: Eq a => a -> [a] -> Bool
elem2 x = foldr (\y res -> (y == x) || res) False

sumaAlt :: [Int] -> Int
sumaAlt = foldr (-) 0

cantApariciones :: Eq a => a -> [a] -> Int
cantApariciones y = foldr (\x rec -> if x == y then rec + 1 else rec) 0

take' :: Int -> [a] -> [a]
{-take2 n l = foldr (\x rec -> if (length rec) < n then x:rec else rec) [] -}
take' = flip take''

take'' :: [a] -> (Int -> [a])
take'' = foldr fLoca (\n -> []) 
         where fLoca = (\x rec -> (\n -> if n <= 0 then [] else x:(rec (n-1))))

--p1e18
data Polinomio a = X
                 |Cte a
                 |Suma (Polinomio a) (Polinomio a)
                 |Prod (Polinomio a) (Polinomio a)
                 --deriving Show

poli :: Polinomio Integer
poli = Suma (Cte 0) (Prod X X) -- p(x) = 0 + x*x

evaluar :: Num a => a -> Polinomio a -> a
evaluar p n = foldPoli n (\c -> c) (\recI recD -> recI + recD) (\recI recD -> recI * recD) p
{-evaluar X n = n -- p(x) = x
evaluar (Cte c) n = c
evaluar (Suma p1 p2) n = (evaluar p1 n) + (evaluar p2 n)
evaluar (Prod p1 p2) n = (evaluar p1 n) * (evaluar p2 n)-}

foldPoli :: b -> -- por x
            (a -> b) -> -- por Cte x
            (b -> b -> b) -> -- suma p1 p2
            (b -> b -> b) -- prod p1 p2
            -> Polinomio a -> b
foldPoli cX fC fS fP p = case p of
	                   X -> cX
	                   (Cte x) -> fC x
	                   (Suma p1 p2) -> fS (rec p1) (rec p2)
	                   (Prod p1 p2) -> fP (rec p1) (rec p2)
	                   where rec = foldPoli cX fC fS fP
{-foldPoli casoX casoCte casoSuma casoProd (X) = casoX
foldPoli casoX casoCte casoSuma casoProd (Cte c) = casoCte c
foldPoli casoX casoCte casoSuma casoProd (Suma p1 p2) = casoSuma (rec p1 rec p2)
              where rec = foldPoli casoX casoCte casoSuma casoProd
foldPoli casoX casoCte casoSuma casoProd (Prod p1 p2) = casoProd rec p1 rec p2)
              where rec = foldPoli casoX casoCte casoSuma casoProd-}

--p1e23
data RoseTree a = Rose a [RoseTree a]
