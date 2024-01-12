module Q2 where
import Cp

-- Predicado para vogais
isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouAEIOU"

reverseByPredicate :: (a -> Bool) -> [a] -> [a]
reverseByPredicate p = rebuildList p.(id><filter p).split id id

outTupleList :: ([a],[a]) -> Either () ([a], [a])
outTupleList ([],_) = i1 ()
outTupleList (list1,list2) = i2(list1,list2)

rebuildList :: (a -> Bool) -> ([a], [a]) -> [a]
rebuildList p = either nil (cond (p.head.p1)
        (cons.(last><rebuildList p.(tail><init)).split p2 id)
        (cons.(head><rebuildList p.(tail><id)).split p1 id)). outTupleList