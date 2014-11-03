module Data.Queue where


data Queue a = Queue [a] [a]

enq :: a -> Queue a -> Queue a
enq x (Queue xs ys) = Queue (x:xs) ys

deq :: Queue a -> (a, Queue a)
deq (Queue [] []) = error "attempt to deq an empty queue."
deq (Queue xs (y:ys)) = (y, Queue xs ys)
deq (Queue xxs@(x:xs) []) = deq $ Queue [] (reverse xxs)
