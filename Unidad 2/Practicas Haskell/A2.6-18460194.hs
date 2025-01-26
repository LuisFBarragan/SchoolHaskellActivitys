-- 2.6 Definición de funciones recursivas --

-- Retorna el máximo valor de una lista --
maximum' :: (Ord a) => [a] -> a
maximum' []     = error "maximum of empty list"
maximum' [x]    = x
maximum' (x:xs) = x `max` (maximum' xs)


-- Retorna una lista con n veces el valor v. --
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x


-- Retorna los primeros n valores de una lista. --
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs


--Retorna la lista inversa de lista. --
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


-- Retorna True si el valor esta en la lista y False si no se encuentra. --
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs


-- lista fibonacci --
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n-2) + fibonacci ( n-1))

listaFibonacci n = [fibonacci x | x <-[0..n]]


-- Retorna una lista ordenada con el método Quicksort --
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- base10abase2 --
par n
   |even n = "0"
   |otherwise = "1"

base2 n
   | n == 1 = "1"
   | n > 1 = (par n) ++ base2 (divi)
   where divi = div n 2

base10base2 n = reverse (base2 n)
