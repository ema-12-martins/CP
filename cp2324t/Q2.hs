module Q2 where
import Cp
import List

-- Predicado para vogais
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p = rebuildList p.(id><filter p).split id id

{- 
rebuildList :: (a -> Bool) -> ([a],[a]) -> [a]
rebuildList _ ([],_) = []
rebuildList p (list,listPredicado)
        | p (head list) = cons (last listPredicado,rebuildList p (tail list,init listPredicado))
        | otherwise = cons (head list,rebuildList p (tail list,listPredicado)) -}

rebuildList :: (a -> Bool) -> ([a], [a]) -> [a]
rebuildList p = cond (p.head.p1)
        (cons.(last><rebuildList p.(tail><init)).split p2 id)
        (cons.(head><rebuildList p.(tail><id)).split p1 id)