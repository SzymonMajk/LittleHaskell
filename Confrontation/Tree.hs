-- funtions that generate a tree with specified height and multiple subtrees, value in node must be equal to value in root, write data and functor:

data MyTree a = Node a [MyTree a] | Empty deriving Show

generateMyTree 0 value = Node value [Empty]
generateMyTree height value = Node value [generateMyTree (height-1) value]

instance Functor MyTree where
  fmap _ Empty = Empty
  fmap f (Node a subTree) = Node (f a) [fmap f x | x <- subTree]
