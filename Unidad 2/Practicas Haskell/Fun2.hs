-- Ejercicios de clase--
--Triple de un numero n^3--     
triple x = x*x*x

--Sacar mitad-- 
mitad x = div (length x) 2

--Impar--                 
impar y = mod (length y) 2 

--Saca valor del medio--
--elemmedio x = if((length x) `mod` 2 == 1) then [x !! (length x `div` 2)] else []--

--Insertar elemento en el meddio--
insElemM x y = (take (div (length x) 2) x)++ [y] ++ (drop(div (length x) 2)x)

--Inserta un elemto donde sea--
--x = Lista, y = PosicionAdd, z =  NumeroAdd--
insElemD x y z= (take y x) ++ [z] ++ (drop y x)

-- Agraga una cadena al final--
elemfin x y = y++x:[]

-- Función de length propia--
length' xs = sum [1|_ <- xs]

--Elimina minusculas--
eliminaMin :: String -> String
eliminaMin x = [c|c <-x, elem c ['A'..'Z']]

--Elimina mayusculas--
eliminaMAY :: String -> String
eliminaMAY x = [c|c <-x, elem c ['a'..'z']]

--Elimina simbolos--
eliminaSim x = [c|c <-x, elem c ['A'..'Z'] || elem c ['a'..'z']]

--Triplas--
tripla :: (a, b, c) -> c
tripla (_, _, z) = z

-- Triangulos--
triangulos a b c = [(a,b,c) | c<- [1..10], b<-[1..10], a<-[1..10], a^2 + b^2 == c^2,  a + b + c == 24]

--Retorna un texto con el nombre del mes y el número del mes.--
diaSemana:: Int-> String
diaSemana s = "El numero " ++ (show s) ++ " corresponde al dia de la semana " ++ case s of {1  -> "Lunes";
                                                                                            2  -> "Martes";
                                                                                            3  -> "Miercoles";
                                                                                            4  -> "Jueves";
                                                                                            5  -> "Viernes";
                                                                                            6  -> "Sabado";
                                                                                            7  -> "Mimingo";
                                                                                            otherwise -> "Error";
                                                                                            }


--SUMA DE VECTORES--

--Head modificada--
head' :: [a]->a
head'[] = error "No puedes utilizar una lista vacia"
head'(x:_) = x
                                                                                            
-- A 2.5 Definición de funciones con guardas, let, case--
--Calcular peso--
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell peso altura
    | peso / altura ^ 2 <= 18.5 = "Peso bajo."
    | peso / altura ^ 2 <= 25.0 = "Peso normal."
    | peso / altura ^ 2 <= 30.0 = "Sobrepeso."
    | otherwise                   = "Ballena!"

--Retorna si el primer valor es igual, menor o igual que el segundo--
miCompara :: (RealFloat a) => a -> a -> String
a `miCompara` b
    | a > b     = "Mayor"
    | a < b     = "Menor"
    | otherwise = "Igual"

--Retorna las raíces de una ecuación cuadrática ax^2 + bx + c--
raicesEcuCuad :: (RealFloat a) => a -> a -> a -> (a,a,String)
raicesEcuCuad a b c
    | raiz < 0 = (0.0,0.0,"i")
    | otherwise = (raiz1,raiz2,"r")
    where raiz = b^2 - 4 * a * c
          raiz1 = ((-b) + sqrt raiz) / (2 * a)
          raiz2 = ((-b) - sqrt raiz) / (2 * a)

--Retorna lista del imc por cada una de las duplas--
calBmis :: (RealFloat a) => [(a, a)] -> [a]
calBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2 

--área de un cilindro --
aCilindro :: (RealFloat a) => a -> a -> a
aCilindro r h =
    let areaB = 2 * pi * r * h
        areaA = pi * r ^2
    in  areaB + 2 * areaA

--Retorna el área total de un tronco de cono, donde r1 es el radio pequeño, r2 el radio grande y g el generatriz--
aTroncoCono :: (RealFloat a) => a -> a -> a -> a
aTroncoCono r1 r2 g =
      let a2 = r2 ^2 + r1 ^2
          a1 = (r2 + r1) * g
      in pi * (a1 + a2)

--Retorna un texto dependiendo de la lista: vacía, unitaria o larga--
describeLista :: [a] -> String
describeLista xs = "La lista es" ++ case xs of {[]  -> " una lista vacia.";
                                                [x] -> " una lista unitaria.";
                                                xs  -> " una lista larga.";
                                                }

--Retorna un texto con el nombre del mes y el número del mes.--
mesAnio:: Int-> String
mesAnio m = "El numero " ++ (show m) ++ " corresponde al mes " ++ case m of {1  -> "Enero";
                                                                         2  -> "Febrero";
                                                                         3  -> "Marzo";
                                                                         4  -> "Abril";
                                                                         5  -> "Mayo";
                                                                         6  -> "Junio";
                                                                         7  -> "Julio";
                                                                         8  -> "Agosto";
                                                                         9  -> "Septiembre";
                                                                         10 -> "Octubre";
                                                                         11 -> "Noviembre";
                                                                         12 -> "Diciembre";
                                                                         otherwise -> "Error";
                                                                         }

-- Clase 2.6 --
--Lista fibo--
--lista x = [| x <-[1..10]]--

-- Fibonacci--
elemFibo :: (Integral a) => a -> a
elemFibo 0 = 0
elemFibo 1 = 1
elemFibo n = elemFibo(n-2) + elemFibo (n-1)

--Factorial--
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)



-- A 2.6 Definición de funciones recursivas --
-- Retorna el máximo valor de una lista--
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Máximo de una lista vacía"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise   = maxTail
    where maxTail = maximum' xs
{-
-- Retorna una lista con n veces el valor v.--
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) xreplicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x -}

-- Retorna los primeros n valores de una lista. --
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- Retorna la lista inversa de lista.--
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- Retorna True si el valor esta en la lista y False si no se encuentra.-- 
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

-- quicksort --
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

--  base10abase2 --
