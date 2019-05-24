data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) 

singleton :: a -> Tree a    
singleton x = Node x EmptyTree EmptyTree    
    
treeInsert :: (Ord a) => a -> Tree a -> Tree a    
treeInsert x EmptyTree = singleton x    
treeInsert x (Node a left right)     
    | x == a = Node x left right    
    | x < a  = Node a (treeInsert x left) right    
    | x > a  = Node a left (treeInsert x right)    

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False 
treeElem x (Node a left right)   
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right 

makeTree :: Ord a => [a] -> Tree a  
makeTree xs = treeListInsert xs EmptyTree  

treeListInsert :: Ord a => [a] -> Tree a -> Tree a  
treeListInsert [] tree = tree  
treeListInsert (x:xs) tree = treeListInsert xs (treeInsert x tree)  

treeToList :: Ord a => Tree a -> [a]  
treeToList EmptyTree = []  
treeToList (Node a left right) = treeToList left ++ [a] ++ treeToList  right  