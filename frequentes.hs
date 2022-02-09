frequentes :: [Int] -> [Int] 
frequentes [] = []
frequentes x  
  | g == r = []
  | otherwise = reposta
  where
    g = length reposta
    r = auxQtdNumero(ordenar x) 
    reposta = comparar(ordenar2(compactar(ordenar x))) 0

compactar :: [Int] -> [(Int, Int)]
compactar []  = []
compactar (x : xs) = (length(contador x xs) + 1, x) : compactar xs 

contador :: Int -> [Int] -> [Int]
contador _ [] = []
contador a (x : xs) 
    | a == x = [a] ++ contador x xs   
    | a /= x =  []  
    | otherwise  = []

comparar :: [(Int, Int)] -> Int -> [Int]
comparar [] _ = []
comparar ((x, y) : xs) m
    | x >= m = [y] ++ comparar xs x   
    | otherwise = []     
-- l: lista 
-- ll : Lista de lista

auxQtdNumero :: [Int] -> Int 
auxQtdNumero (x:xs)  = qtdNumero xs x 

qtdNumero :: [Int] -> Int  -> Int
qtdNumero [] _ = 1
qtdNumero (x: xs) b 
    | b /= x = 1 + qtdNumero xs x     
    | otherwise =  qtdNumero xs x 

ordenar :: [Int] -> [Int] 
ordenar [] = []
ordenar (a : x) = ordenar[b | b <- x, b <= a] ++ [a] ++ ordenar[b | b <- x, b > a]

ordenar2 ::  [(Int, Int)] ->  [(Int, Int)]
ordenar2 [] = []
ordenar2 (a : x) = ordenar2[b | b <- x, b > a] ++ [a] ++ ordenar2[b | b <- x, b <= a]

