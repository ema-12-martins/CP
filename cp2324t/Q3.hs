module Q3 where
import Data.List
import Cp
import Nat
----------------- Versao pointwise a funcionar--------------------------------
snh x = wrapper . worker where
        worker = for (loop x) (start x)
        wrapper (a,_,_) = a

loop x (f,k,g)= (f + k,k * ((x ^ 2)/((g+1)*(g+2))),g+2)
start x = (x,(x ^ 3)/6,3)