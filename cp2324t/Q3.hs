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

loop n (f,k,g)= (f + k,k * ((n ^ 2)/((g+2)*(g+3))),g+1)
start n = (n,n,0)
