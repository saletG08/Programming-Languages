data BTree a = Empty
              | Node a (BTree a) (BTree a)
              deriving (Show, Eq)

instance Functor BTree where
    fmap _ Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Applicative BTree where
    pure x = Node x Empty Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    (Node f leftF rightF) <*> (Node x leftX rightX) = Node (f x) (leftF <*> leftX) (rightF <*> rightX)

incrementTree :: BTree Int -> BTree Int
incrementTree = fmap (+1)


contar :: BTree String -> BTree Int
contar = fmap (length)

{- mergeTrees :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
mergeTrees _ Empty Empty = Empty
mergeTrees f (Node x leftX rightX) (Node y leftY rightY) = Node (f x y) (mergeTrees f leftX leftY) (mergeTrees f rightX rightY)
mergeTrees _ _ _ = error "Trees do not have the same structure"

mergeStringTrees :: BTree String -> BTree String -> BTree String
mergeStringTrees = mergeTrees (\str1 str2 -> str1 ++ " " ++ str2) -}

--- Primero

insertar :: Ord a => a -> BTree a -> BTree a
insertar x Empty = Node x Empty Empty
insertar x (Node y left right)
    | x < y     = Node y (insertar x left) right
    | otherwise = Node y left (insertar x right)

fusionarArboles :: Ord a => BTree a -> BTree a -> BTree a
fusionarArboles arbol1 Empty = arbol1
fusionarArboles arbol1 (Node x left right) =
    fusionarArboles (insertar x arbol1) (fusionarArboles left right)

arbolA :: BTree Int
arbolA = Node 10 (Node 5 Empty Empty) (Node 15 Empty Empty)

arbolB :: BTree Int
arbolB = Node 20 (Node 17 Empty Empty) (Node 25 Empty Empty)

arbol3 :: BTree Int
arbol3 = fusionarArboles arbolA arbolB

--- Segundo

mergeTrees :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
mergeTrees f arbol1 arbol2 = fmap f arbol1 <*> arbol2

arbol4 :: BTree String
arbol4 = Node "Hola" (Node "como" Empty Empty) (Node "bien" Empty Empty)

arbol5 :: BTree String
arbol5 = Node "Mundo" (Node "estas" Empty Empty) (Node "gracias" Empty Empty)

arbol6 :: BTree String
arbol6 = mergeTrees (++) arbol4 arbol5

---

addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes mx my = (+) <$> mx <*> my