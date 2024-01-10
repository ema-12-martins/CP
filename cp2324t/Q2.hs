module Q2 where

reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p [] = []
reverseByPredicate p (h:t)  |p h = reverseByPredicate p t ++ [h]
                            |otherwise = h: reverseByPredicate p t