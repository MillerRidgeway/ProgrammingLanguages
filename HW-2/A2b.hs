module A2b where 
import Data.List

--List functions
--Removes all members of a list except the specified singleton
removeAllExcept :: Eq a => a -> [a] -> [a]
removeAllExcept item list = 
  case list of 
    [] -> []
    _ -> if (head list) == item
          then [item] ++ removeAllExcept item (tail list) 
          else removeAllExcept item (tail list)

--Removes all members of a list that match the specified singleton
removeAll :: Eq a => a -> [a] -> [a]
removeAll item list =
  case list of 
    [] -> []
    _ -> if (head list) == item
          then removeAll item (tail list)
          else [head list] ++ removeAll item (tail list)

--Substitues the specified value in for all instances of the specified singleton
substitute :: Eq a => a -> a -> [a] -> [a]
substitute old new list = 
  case list of 
    [] -> []
    _ -> if (head list) == old
          then [new] ++ substitute old new (tail list)
          else [head list] ++ substitute old new (tail list)

--Sorts 3 pre-sorted lists from least to greatest
mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 [] [] [] = 
  []

mergeSorted3 list1 list2 list3 = 
  let 
    composite = list1 ++ list2 ++ list3
    item = minimum composite
    newList = delete item composite
  in
    [item] ++ mergeSorted3 newList [] []

-- Tree Functions

data TriTree a = EmptyNode | TriNode a (TriTree a) (TriTree a) (TriTree a)
    deriving Show
  
instance (Eq a) => Eq (TriTree a) where
  EmptyNode           == EmptyNode = True
  TriNode a la ma ra  == TriNode b lb mb rb = (a == b) &&
                                              (la == lb) &&
                                              (ma == mb) &&
                                              (ra == rb)
  _                   == _ = False

--Returns the value contained in a given TriTree. Cannot be called on an empty tree.
nodeValue :: TriTree a -> a
nodeValue EmptyNode = error "Cannot call nodeValue on empty node"
nodeValue (TriNode val left middle right) = val

--Returns the left child TriTree of a given TriTree. Cannot be called on an empty tree.
leftChild :: TriTree a -> TriTree a
leftChild EmptyNode = error "Cannot call leftChild on empty node"
leftChild (TriNode val left middle right) = left
--Returns the right child TriTree of a given TriTree. Cannot be called on an empty tree.
rightChild :: TriTree a -> TriTree a
rightChild EmptyNode = error "Cannot call rightChild on empty node"
rightChild (TriNode val left middle right) = right

--Returns a boolean indicating whether or not a value is present within a given TriTree.
inTree :: Eq a => a -> TriTree a -> Bool
inTree item EmptyNode = False
inTree item (TriNode val left middle right)
  | item == val = True
  | otherwise   = inTree item left || inTree item middle || inTree item right

--Returns a list containing all leaf-node values in a given TriTree
leafList :: TriTree a -> [a]
leafList tree = 
  case tree of 
    EmptyNode                                  -> []
    TriNode item EmptyNode EmptyNode EmptyNode -> item:[]
    TriNode _ left middle right                -> 
      (leafList left) ++ (leafList middle) ++ (leafList right)

--Maps a given function over a given TriTree in order
inOrderMap :: (a -> b) -> TriTree a -> TriTree b
inOrderMap func tree = apply tree
  where apply EmptyNode = EmptyNode
        apply (TriNode item EmptyNode EmptyNode EmptyNode) = TriNode (func item) EmptyNode EmptyNode EmptyNode
        apply (TriNode item left middle right) = TriNode (func item) (apply left) (apply middle) (apply right) 

--Folds a given function over a given TriTree in pre-order  
preOrderFold :: (b -> a -> b) -> b -> TriTree a -> b
preOrderFold func base tree = 
  case tree of 
    EmptyNode                                  -> base 
    TriNode item left middle right             -> 
      func (preOrderFold func accum right) item
      where 
        accum = preOrderFold func accumLeft middle 
        accumLeft = preOrderFold func base left