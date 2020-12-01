type Nodo = Integer
data Grafo = G [Nodo] (Nodo -> [Nodo])

grafoSinCiclos :: Grafo
grafoSinCiclos = G [1,2,3,4] fGrafoSinCiclos

fGrafoSinCiclos :: Nodo -> [Nodo]
fGrafoSinCiclos 1 = [2]
fGrafoSinCiclos 2 = [3]
fGrafoSinCiclos 3 = [4]
fGrafoSinCiclos 4 = []

--a
ejGrafo :: Grafo
ejGrafo = G [1..9] fEjGrafo

fEjGrafo :: Nodo -> [Nodo]
fEjGrafo n | (even n) = [1..9]
           | otherwise = []

--b
--pre: n \in ns
nodoReflex :: Grafo -> Nodo -> Bool
nodoReflex (G ns f) n = elem n (f n)
    -- |(elem n (f n)) = True --resol original
    -- |otherwise = False

--c
extenderCamino :: Grafo -> [Nodo] -> [[Nodo]]
extenderCamino (G ns f) path = map (agregarAlFinal path) (f (last path))

agregarAlFinal :: [Nodo] -> Nodo -> [Nodo]
agregarAlFinal ns n = ns ++ [n]

--d
caminosDeLong :: Grafo -> Int -> [[Nodo]]
caminosDeLong (G ns f) i = foldNat (extenderCaminoParaTodos (G ns f)) (nodosEnListas ns) i 
-- resolucion: foldNat [[n] <- ns] (concatMap (extenderCamino g)) 
-- g@(G ns f)
nodosEnListas :: [Nodo] -> [[Nodo]]
nodosEnListas = map (\x -> [x])

extenderCaminoParaTodos :: Grafo -> [[Nodo]] -> [[Nodo]]
extenderCaminoParaTodos g ns = concat (map (extenderCamino g) ns)

foldNat :: (a -> a) -> a -> Int -> a
foldNat f z 0 = z
foldNat f z m = f (foldNat f z (m-1))

--e
hamiltoniano :: Grafo -> Bool
hamiltoniano (G ns f) = foldr fHamiltoniano False (caminosDeLongSinRepetir (G ns f) ((length ns)-1))
                        where fHamiltoniano path rec = if rec then True else (tieneTodosLosNodos path)
                              tieneTodosLosNodos = foldr (\x rec -> rec && (elem x ns)) True

--otra res: any sinRepetidos $ caminosDeLong g (long ns - 1)

caminosDeLongSinRepetir :: Grafo -> Int -> [[Nodo]]
caminosDeLongSinRepetir (G ns f) i = foldNat (extenderCaminoParaTodosSinRepetir (G ns f)) (nodosEnListas ns) i

extenderCaminoParaTodosSinRepetir :: Grafo -> [[Nodo]] -> [[Nodo]]
extenderCaminoParaTodosSinRepetir g ns = concat (map (extenderCaminoSinRepetir g) ns)

extenderCaminoSinRepetir :: Grafo -> [Nodo] -> [[Nodo]]
extenderCaminoSinRepetir (G ns f) path = map (agregarAlFinal path) (adyacentesNoEnCamino path (f (last path)) )
                                    where adyacentesNoEnCamino path = filter (\n -> not (elem n path)) 

--f
tieneCiclo :: Grafo -> Bool
tieneCiclo (G ns f) = (caminosDeLong (G ns f) (length ns)) /= []
-- not $ null $ caminosDeLong g (long ns)
--g
caminos :: Grafo -> [[Nodo]]
caminos (G ns f) | tieneCiclo g = concat [caminosDeLong g n | n <- [0..]]
                 | otherwise = concat [caminosDeLong g n | n <- [0..((length ns)-1)]]
                where g = G ns f


