module Iterator where

class Iterator i where
  iterMap :: (a -> b) -> i a -> i b
  iterFold :: (a -> b -> a) -> a -> i b -> a

data List a = Cons a (List a)
            | Nil deriving (Eq, Show)

data Tree a = Branch a (List (Tree a))
            | Leaf deriving (Eq, Show)

data Queue a = Queue (List a) (List a) deriving (Show)

instance (Eq a) => Eq (Queue a) where
    (==) (Queue Nil ys) (Queue Nil xs) = ys == xs
    (==) q r = (rebalance q) == (rebalance r)

enqueue :: Queue a -> a -> Queue a
enqueue (Queue inbox outbox) x = Queue (Cons x inbox) outbox

rebalance (Queue Nil ys) = Queue Nil ys
rebalance (Queue (Cons x xs) ys) = rebalance (Queue xs (Cons x ys))

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue Nil Nil) = (Nothing, Queue Nil Nil)
dequeue (Queue xs (Cons y ys)) = (Just y, Queue xs ys)
dequeue (Queue xs Nil) = dequeue (rebalance (Queue xs Nil))

instance Iterator List where
     iterMap func Nil = Nil
     iterMap func (Cons x xs) = Cons (func x) (iterMap func xs)

     iterFold func init Nil = init
     iterFold func init (Cons x xs) = iterFold func (func init x) xs
instance Iterator Tree where
     iterMap func Leaf = Leaf
     iterMap func (Branch x xs) = Branch (func x) (iterMap (iterMap func) xs)
     
     iterFold func init Leaf = init
     iterFold func init (Branch x xs) = iterFold (iterFold func) (func init x) xs
instance Iterator Queue where
     iterMap func (Queue x y) = Queue (iterMap func x) (iterMap func y)

     iterFold func init (Queue x y) = iterFold func (iterFold func init x) y
