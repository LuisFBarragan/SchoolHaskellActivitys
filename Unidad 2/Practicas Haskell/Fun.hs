import Data.Char(digitToInt)

-- convierte bin--
binADem num = sum [a*b | (a,b) <- zip [digitToInt  d | d <- reverse (show num)][2^p | p <- [0..length (show num)]]]

-- Complemento a uno --
compUno [] = "Lista vacia"
compUno ['0'] = ['1']
compUno ['1'] = ['0']
compUno ('0':xs) = ['1'] ++ (compUno xs)
compUno ('1':xs) = ['0'] ++ (compUno xs)

{-
--contar
--Elimina simbolos--
eliminaSim x = [c|c <-x, elem c ['A'..'Z'] || elem c ['a'..'z']]

-- Elimina letras --
-- eliminaLetras x = [c|c <-x, elem c [0..9]] --


--determina cant de letras, numeros y simbolos --
{-queHay :: (String a) => a -> [a]
queHay [] = "Lista vacia"
queHay (x:xs)
    | eliminaSim 
    | eliminaLetras
    | otherwise 
-}

--contar
contar [] = "No hay caracteres"
contar [a] = [x+1,y,z]
contar [b] = [x,y+1,z]

where a `elem` ['A'..'Z'] ++ ['a'..'z']
-}

