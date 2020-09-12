-- ej 2
valorAbsoluto :: Float -> Float
valorAbsoluto x = if (x >= 0) then x else -x

bisiesto :: Int -> Bool
bisiesto x = if ((mod x 4) /= 0) then False else True

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x + factorial (x-1)

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos 0 = 0
cantDivisoresPrimos 1 = 0
cantDivisoresPrimos x = cantDivisoresPrimosAux x x 0

cantDivisoresPrimosAux :: Int -> Int -> Int -> Int
cantDivisoresPrimosAux x 2 z = if (x `esDivididoPor` 2) then (1 + z) else z
cantDivisoresPrimosAux x y z = if (esPrimoYDivide y x) then (cantDivisoresPrimosAux x (y-1) (z+1)) else (cantDivisoresPrimosAux x (y-1) z)

esDivididoPor :: Int -> Int -> Bool
esDivididoPor x y = mod x y == 0

esPrimo :: Int -> Bool
esPrimo 0 = False
esPrimo 1 = False
esPrimo 2 = True
esPrimo x = esPrimoAux x (x-1)

esPrimoAux :: Int -> Int -> Bool
esPrimoAux x 1 = True
esPrimoAux x y = if (x `esDivididoPor` y) then False else (esPrimoAux x (y-1))

esPrimoYDivide :: Int -> Int -> Bool
esPrimoYDivide y x = (esPrimo y) && (x `esDivididoPor` y)

-- ej 3
-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right True) = 1
aEntero (Right False) = 0

-- ej 4
limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar (x:xs) ys = limpiar xs (limpiarUno x ys)

limpiarUno :: Char -> String -> String
limpiarUno a [] = []
limpiarUno a (x:xs) = if (a == x) then (limpiarUno a xs) else (x:(limpiarUno a xs))

difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio xs = difPromedioAux xs (promedio xs)

promedio :: [Float] -> Float
promedio xs = promedioAux xs 0 0

promedioAux :: [Float] -> Float -> Float -> Float
promedioAux [] sum size = sum/size
promedioAux (x:xs) sum size = promedioAux xs (sum + x) (size + 1)

difPromedioAux :: [Float] -> Float -> [Float]
difPromedioAux [] avg = []
difPromedioAux (x:xs) avg = (x-avg):(difPromedioAux xs avg)

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [a] = True
todosIguales (x:xs) = if (x == (head xs)) then (todosIguales xs) else (False)

--ej 5

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin a1 a a2) = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin a1 a a2) = Bin (negacionAB a1) (not a) (negacionAB a2)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin a1 a a2) = productoAB a1 * a * (productoAB a2)