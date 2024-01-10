module Q2 where
import Cp
import List


{- Versao PointWise
reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p [] = []
reverseByPredicate p (h:t)  |p h = reverseByPredicate p t ++ [h]
                            |otherwise = h: reverseByPredicate p t -}
                            

reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p = either nil (cond (p.p1)
    (conc.split (reverseByPredicate p.p2) (singl.p1))
    (cons.split p1 (reverseByPredicate p.p2))) .outList