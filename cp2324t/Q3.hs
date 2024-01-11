import Data.List
import Cp
import Nat

{- wrapper(a,_,_) = a
f' x i = wrapper . for loop init where
        loop (f,k,g) = (f + k,k * ((x ^ 2)/g),g+2)
        init = (zero,x,one) -}


snh' x = id . worker where
        wrapper(a,_,_) = a
        worker = for (loop x) (start x)

loop x (f,k,g)= (f + k,k * ((x ^ 2)/((g+1)*(g+2))),g+2)
start x = (x,(x ^ 3)/6,3)
