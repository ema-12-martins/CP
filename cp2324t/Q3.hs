import Data.List
import Cp
import Nat

------- Funcao necessaria para ambas as versoes------------------------
wrapper(a,_,_) = a

--------------Versao pointfree a funcionar (ou o mais perto de ser)----------------

snh2 x = wrapper . for loop init where
        loop (f,k,g)= (f + k,k * ((x ^ 2)/((g+1)*(g+2))),g+2)
        init = (x,(x ^ 3)/6,3)
----------------- Versao pointwise a funcionar--------------------------------

snh' x = wrapper . worker where
        worker = for (loop x) (start x)

loop x (f,k,g)= (f + k,k * ((x ^ 2)/((g+1)*(g+2))),g+2)
start x = (x,(x ^ 3)/6,3)
