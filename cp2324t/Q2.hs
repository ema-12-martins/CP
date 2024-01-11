module Q2 where
import Cp
import List


{- Versao PointWise Errada
reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p [] = []
reverseByPredicate p (h:t)  |p h = reverseByPredicate p t ++ [h]
                            |otherwise = h: reverseByPredicate p t -}

{- Versao PointFree errada
reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p = either nil (cond (p.p1)
    (conc.split (reverseByPredicate p.p2) (singl.p1))
    (cons.split p1 (reverseByPredicate p.p2))) .outList -}


-- Testar para Vogais
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

{- Versao PointWise
reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p [] = []
reverseByPredicate p lst = rebuildList p lst (filter p lst) -}

rebuildList :: (a -> Bool) -> [a] -> [a] -> [a]
rebuildList _ [] _ = []
rebuildList p (x:xs) listPredicado
        | p x = last listPredicado : rebuildList p xs (init listPredicado)
        | otherwise = x : rebuildList p xs listPredicado