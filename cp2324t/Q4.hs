import System.Random
import Probability
import Data.List

--------------------------DADOS DO PROBLEMA----------------------------------
data Stop = S0 |S1 |S2 |S3 |S4 |S5 deriving (Show,Eq,Ord,Enum)
type Segment = (Stop,Stop)
type Delay = Int

dados :: [(Segment,Delay)]
dados = [((S0,S1),0),((S0,S1),2),((S0,S1),0),((S0,S1),3),((S0,S1),3),
    ((S1,S2),0),((S1,S2),2),((S1,S2),1),((S1,S2),1),((S1,S2),4),
    ((S2,S3),2),((S2,S3),2),((S2,S3),4),((S2,S3),0),((S2,S3),5),
    ((S3,S4),2),((S3,S4),3),((S3,S4),5),((S3,S4),2),((S3,S4),0),
    ((S4,S5),0),((S4,S5),5),((S4,S5),0),((S4,S5),7),((S4,S5),-1)]

---------- Exercicio propriamente dito -------------------

groupTuples :: [(Segment, Delay)] -> [[(Segment, Delay)]]
groupTuples = groupBy (\(x, _) (y, _) -> x == y)

aux :: [(Segment, Delay)] -> [(Segment,Dist Delay)]
aux lista = [(fst (head lista),distribuicao)]
  where
    delays = map snd lista
    delaysWithout = nub delays
    tamanho = length lista
    countOccurrences item list = length (filter (== item) list)
    distribuicao = D [(delay, fromIntegral (countOccurrences delay delays) / fromIntegral tamanho) | delay <- delaysWithout]

--Gerar base de dados de probabilidades
bd :: [(Segment, Delay)] -> [(Segment,Dist Delay)]
bd list = concatMap aux (groupTuples list)

-- Com a bd, definir funcao de probabilidade
delay :: Segment -> Dist Delay



--Dar o delay acomulado do percurso
{- pdelay :: Stop -> Stop -> Dist Delay
pedelay x x = return 0
pedelay x y = do{
    x <- delay;
    y <- pedelay 
} -}



