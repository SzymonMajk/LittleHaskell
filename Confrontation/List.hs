--myList with map, foldr, foldl, zipWith, ++, and conversion from and to normal list

data MyList a = Cons a (MyList a) | Nil deriving Show

instance Functor MyList where
  fmap _ Nil = Nil
  fmap f (Cons a list) = Cons (f a) (myMap f list)

instance Applicative MyList where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons a lista) <*> (Cons b listb) = Cons (a b) (lista <*> listb)

myMap _ Nil = Nil
myMap f (Cons a list) = Cons (f a) (myMap f list)

myFoldr _ acc Nil = acc
myFoldr f acc (Cons a list) = f a (myFoldr f acc list)

myFoldl _ acc Nil = acc
myFoldl f acc (Cons a list) = myFoldr f (f acc a) list

myZipWith _ Nil Nil = Nil
myZipWith f (Cons a lista) (Cons b listb) = Cons (f a b) (myZipWith f lista listb)

concatt Nil b = b
concatt (Cons a lista) listb = Cons a (concatt lista listb)

fromNormalList [] = Nil
fromNormalList (x:xs) = Cons x (fromNormalList xs)

toNormalList Nil = []
toNormalList (Cons a list) = a:(toNormalList list)
