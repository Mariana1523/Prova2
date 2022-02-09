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
qtdNumero [] _ = 0
qtdNumero (x: xs) b 
    | b == x = qtdNumero xs b 
    | otherwise = 1 + qtdNumero xs b 

ordenar :: [Int] -> [Int] 
ordenar [] = []
ordenar (a : x) = ordenar[b | b <- x, b <= a] ++ [a] ++ ordenar[b | b <- x, b > a]

ordenar2 ::  [(Int, Int)] ->  [(Int, Int)]
ordenar2 [] = []
ordenar2 (a : x) = ordenar2[b | b <- x, b > a] ++ [a] ++ ordenar2[b | b <- x, b <= a]

-- repete(N, [X,Y|Zs], Ls) :- X = Y, W is N + 1, repete(W, [Y|Zs], Ls).
-- repete(N, [X,Y|Zs], [[W, X]|Ls]) :- X \= Y, W is N + 1, repete(0, [Y|Zs], Ls).


-- quantidade :: a -> [a] -> [a]
-- quantidade _ [] = []
-- quantidade e (x : xs)
--   | e x = x : quantidade e xs
--   | otherwise = quantidade e xs

--   --Frequentes [c|L] = compactar [c|L], frequentes [L]
