-- Construcón de árbol --
data Arbol a = Hoja
   | Nodo a (Arbol a) (Arbol a)
   deriving (Show, Eq)


arbola = Nodo 10 (Nodo 15 
                         (Nodo 24 Hoja Hoja)
                         (Nodo 27 Hoja Hoja))
                  (Nodo 18
                          Hoja (Nodo 24 Hoja Hoja))

arbolb = Nodo 50 (Nodo 30 
                         (Nodo 30 Hoja Hoja)
                         (Nodo 35 Hoja Hoja))
                  (Nodo 60
                          Hoja (Nodo 80 Hoja Hoja))
arbole = Hoja

-- Raiz de un árbol --
raizB Hoja      = error "Arbol vacio"
raizB (Nodo x _ _) = x

-- Número de hojas de un árbol
nHojas :: Arbol a -> Int
nHojas Hoja = 1
nHojas (Nodo x i d) = nHojas i + nHojas d

-- Retorna el número de nodos del árbol --
tamañoB :: Arbol a -> Integer
tamañoB Hoja = 0
tamañoB (Nodo x i d) = 1 + tamañoB i + tamañoB d

-- Retorna el número de niveles de un árbol --
profundidadB :: Arbol a -> Int
profundidadB Hoja = 0
profundidadB (Nodo x i d) = 1 + max (profundidadB i) (profundidadB d)

-- Retorna el recorrido en entre orden de un árbol --
enOrdenB :: Arbol a -> [a]
enOrdenB Hoja = []
enOrdenB (Nodo x i d) = enOrdenB i ++ [x] ++ enOrdenB d

-- Retorna el recorrido en pre orden de un árbol --
preOrdenB :: Arbol a -> [a]
preOrdenB Hoja = []
preOrdenB (Nodo x i d) = x : (preOrdenB i ++ preOrdenB d)

-- Retorna el recorrido en post orden de un árbol --
postOrdenB :: Arbol a -> [a]
postOrdenB Hoja = []
postOrdenB (Nodo x i d) = postOrdenB i ++ postOrdenB d ++ [x]

-- Suma los valores de los nodos del árbol --
sumaArbolB :: Arbol Integer -> Integer
sumaArbolB Hoja =0
sumaArbolB (Nodo x i d) = suma (sumaArbolB i) x (sumaArbolB d)
  where
     suma a b c = a + b + c
-- Retorna True si es un arbol de búsqueda y False si no lo es --
todosArbolB a Hoja = True
todosArbolB a (Nodo x i d) = a x &&
                             todosArbolB a i && todosArbolB a d

esArbolBB :: Ord a => Arbol a -> Bool
esArbolBB Hoja = True
esArbolBB (Nodo x i d) = todosArbolB(<= x) i
                         && todosArbolB (>x) d
                         && esArbolBB i
                         && esArbolBB d

-- Retorna True si el número se encuentra en el árbol binario de búsqueda --
perteneceBB n Hoja = False
perteneceBB n (Nodo x i d)
   | n == x = True
   | n < x = perteneceBB n i
   | otherwise = perteneceBB n d

--Función para insertar un elemento en el arbol binario --
insertarBB n Hoja = Nodo n Hoja Hoja
insertarBB n (Nodo x i d)
   | n <= x = Nodo x (insertarBB n i) d
   | otherwise = Nodo x i (insertarBB n d)

-- Construye un arbol de busqueda a partir de los valores de una lista --
listaAArbolBB :: Ord a => [a] -> Arbol a
listaAArbolBB n = foldr insertarBB Hoja n

-- Devuelvelista ordenada aplicando el recorrido de arboles --
arbolOrdenado :: Ord a => [a] ->[a]
arbolOrdenado = enOrdenB.listaAArbolBB
