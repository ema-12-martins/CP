module Q2 where
import Cp
import List


{- Versao PointWise Errada
reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p [] = []
reverseByPredicate p (h:t)  |p h = reverseByPredicate p t ++ [h]
                            |otherwise = h: reverseByPredicate p t -}

{- Versao PointFree Errada
reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p = either nil (cond (p.p1)
    (conc.split (reverseByPredicate p.p2) (singl.p1))
    (cons.split p1 (reverseByPredicate p.p2))) .outList -}


-- Predicado para vogais
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p = rebuildList p.(id><filter p).split id id


rebuildList :: (a -> Bool) -> ([a],[a]) -> [a]
rebuildList _ ([],_) = []
rebuildList p (h:t,listPredicado)
        | p h = last listPredicado : rebuildList p (t,init listPredicado)
        | otherwise = h : rebuildList p (t,listPredicado)