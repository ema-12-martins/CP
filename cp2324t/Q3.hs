import Data.List
import Cp
import Nat
{-
O do fatorial ficava assim entao:

int fac(int n) {
    int x=1; 
    int y=1;
    
    int j;
    for (j=1;j<n+1;j++) {
        y=x*y;
        x=x+1;
        }
    return y;
};
-}

fibMeu = p2 . (for g (1, 1)) where g (x, y ) = (x + 1, x * y )

{-
Em C, o programa ficava assim:

double seno_hiperbolico(double x, int n) {
    int x=0; 
    int y=1;
    int i;
    for (i = 0; i < n; i++) {
        y=(pow(x, 2 * i + 1) / fatorial(2 * i + 1))*y;
        x=x+1;
    }
    return senh;
}
-}

shMeu = p2 . (for g (0, 1)) where g (x, y) = (x + 1, (pow(x, 2 * i + 1) / fatorial(2 * i + 1)) + y)

{-
Em C, o programa ficava assim:

double seno_hiperbolico(double x, int n) {
    double senh = 0;
    int i;
    for (i = 0; i < n; i++) {
        senh += (pow(x, 2 * i + 1) / fatorial(2 * i + 1));
    }
    return senh;
}
-}