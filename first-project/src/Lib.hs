module Lib
    (  inc,
    quadrado,
    soma,
    mult,
    fac,
    pow,
    lstvezes,
    invertelst,
    powlst,
    foo,
    somalst1,
    somalst2,
    somalst3,
    somalst4,
    somatorio,
    norma,
    incCurried,
    somaCurried,
    dobra1,
    mergelst,
    produtosInternos,
    prod_int,
    sim,
    functionTest,
    --lab #03 - Guards, cases, etc
    tipoletra,
    replica,
    par,
    pares,
    getnOfSqrList,
    getnOfPow2List,
    getnOfFibonacci

        ) where

inc :: Int -> Int
inc n = n + 1

quadrado :: Int -> Int
quadrado n = n * 2

soma :: Int -> Int -> Int
soma a b = a + b

fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x-1)

mult :: Int -> Int -> Int
mult x y = x * y

pow :: Integer -> Integer -> Integer
pow _ 0 = 1 
pow n e = n * pow n (e-1)

lstvezes :: [Int] -> Int -> [Int]
lstvezes list n = [ (mult x n)  | x <- list]

invertelst [] = []
invertelst (head_value:tail_list) = invertelst tail_list ++ [head_value]


powlst :: [Integer] -> Integer -> [Integer]
powlst list n = [ (pow x n)  | x <- list] 
 

foo :: Int -> Int
foo x = x * x

functionTest :: (a->a) -> a -> a
functionTest x y = x y

somaCurried = (\x -> \y -> x+y)

incCurried x = somaCurried 1 x

somalst1 list = foldl (+) 0 list 
somalst2 list = foldr (+) 0 list
somalst3 list = foldl (\x -> \y -> x+y) 0 list
somalst4 list = foldr (\x -> \y -> x+y) 0 list

somatorio list = foldl (\x -> \y -> x+y) 0 list


dobra1 list = map ((\x -> \y -> x*y)2) list

norma list = sqrt (foldl (\x -> \y-> x+y) 0 (map (\x->x*x) list))

--mergelst (x:xs) (y:ys)= (x : ys) : 1

mergelst [] ys = ys
mergelst (x:xs) (ys) = x:mergelst xs (invertelst ys)

produtosInternos (x:xs) (y:ys) = x * y : produtosInternos xs ys
produtosInternos _ _ = []

prod_int list1 list2 = foldl (\x -> \y -> x+y) 0 (produtosInternos list1 list2)

sim list1 list2 = (prod_int list1 list2) / (norma list1 * norma list2)   


tipoletra l
           | l >= 'a' && l <= 'z' = "minuscula"
           | l >= 'A' && l <= 'Z' = "maiuscula"
           | otherwise = "Nao eh uma letra ASCII!"


replica e n 
           | n < 0 = error "n deve ser maior ou igual a zero!"
           | n == 0 = []
           | otherwise = e: replica e (n-1)

--Função que devolve o enésimo número par:    
par n = pares!!n where pares = 0:map (+2) pares

--Função que devolve os n primeiros números pares:
pares n = take n pares where pares = 0:map (+2) pares

getnOfSqrList n = take n sqrList where sqrList = 2:map (\x -> x*x) sqrList

getnOfPow2List n = take n pow2List where pow2List = 2:map (\x -> x*2) pow2List

getnOfFibonacci n = take n fibonacci where fibonacci = map fst (iterate (\(a,b) -> (b,a+b)) (0,1))
                                                    

