module Data.Queue where

import Control.Applicative

data Queue a = Queue [a] [a] deriving Show

enq :: a -> Queue a -> Queue a
enq x (Queue xs ys) = Queue (x:xs) ys

deq :: Queue a -> (a, Queue a)
deq (Queue [] []) = error "attempt to deq an empty queue."
deq (Queue xs (y:ys)) = (y, Queue xs ys)
deq (Queue xxs@(x:xs) []) = deq $ Queue [] (reverse xxs)


emptyQ :: Queue a
emptyQ = Queue [] []


toList :: Queue a -> [a]
toList (Queue xs ys) = ys ++ reverse xs


fromList :: [a] -> Queue a
fromList xs = Queue xs []


takeQ :: Int -> Queue a -> [a]
takeQ 0 _ = []
takeQ n q = x : takeQ (n-1) q'
    where
        (x, q') = deq q


manyQ :: Alternative f => f a -> f (Queue a)
manyQ v = manyQ_v
    where
        manyQ_v = someQ_v <|> pure emptyQ
        someQ_v = enq <$> v <*> manyQ_v


someQ :: Alternative f => f a -> f (Queue a)
someQ v = someQ_v
    where
        manyQ_v = someQ_v <|> pure emptyQ
        someQ_v = enq <$> v <*> manyQ_v


instance Functor Queue where
    fmap f (Queue xs ys) = Queue (fmap f xs) (fmap f ys)
