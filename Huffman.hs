import Data.Map

type Value = Int
type Key = [Char]
type NodeValue = (Key,Value)

data Heap_ a = Empty
        | Node a (Heap_ a) (Heap_ a)
        deriving(Show, Eq)

type Heap a = Heap_ NodeValue

frequencyOfCharacters :: [Char] -> Map Key Value
frequencyOfCharacters [] = Data.Map.empty
frequencyOfCharacters (character:text) = insertWith (+) [character] 1 (frequencyOfCharacters text)

makeLeaf :: NodeValue -> Heap a
makeLeaf a = Node a Empty Empty

mergeHeaps :: Heap a -> Heap a -> Heap a
mergeHeaps Empty rightHeap = rightHeap
mergeHeaps leftHeap Empty = leftHeap
mergeHeaps leftHeap@(Node a lefta righta) rightHeap@(Node b leftb rightb)
    | snd a < snd b = Node a (mergeHeaps lefta rightHeap) righta
    | otherwise = Node b leftb (mergeHeaps leftHeap rightb)

addToHeap :: Heap a->NodeValue->Heap a
addToHeap Empty a =  makeLeaf a
addToHeap h a = mergeHeaps h (makeLeaf a)


takeHeadFromHeap :: Heap a -> (NodeValue,Heap a)
takeHeadFromHeap Empty = (("",-1), Empty)
takeHeadFromHeap (Node a leftBranch rightBranch) = (a, mergeHeaps leftBranch rightBranch)

makeHeap :: Map Key Value -> Heap a
makeHeap map_ = makeHeap_ $ toList map_

makeHeap_ :: [(Key,Value)] -> Heap a
makeHeap_ [] = Empty
makeHeap_ (x:xs) = addToHeap (makeHeap_ xs) x


huffmanEntry :: [Char]-> Heap a
huffmanEntry text = makeHeap $ frequencyOfCharacters text

data Huff = Leaf Double Char | HNode Double Huff Huff
data Huff' freq tok = Leaf' freq tok | Node' (Huff' freq tok) (Huff' freq tok)