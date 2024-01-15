module Q4 where
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


dbDistAux :: [(Segment, Delay)] -> [(Segment, Dist Delay)]
dbDistAux lista = do
  let delays = map snd lista
      delaysWithout = nub delays
      tamanho = length lista

  let countOccurrences item list = length (filter (== item) list)
      distribuicao = D [(delay, fromIntegral (countOccurrences delay delays) / fromIntegral tamanho) | delay <- delaysWithout]

  return (fst (head lista), distribuicao)


dbDist :: [(Segment, Delay)] -> [(Segment,Dist Delay)]
dbDist list = concatMap dbDistAux (groupTuples list)


--Gerar base de dados de probabilidades
db :: [(Segment,Dist Delay)]
db = dbDist dados


--Dar a distribuicao de delay num segmento
delay :: Segment -> Dist Delay
delay seg = case lookup seg db of
              Just dist -> dist
              Nothing   -> D [] 


--Dar a distribuicao de delay entre duas paragens
pdelay :: Stop -> Stop -> Dist Delay
pdelay s1 s2 = delay (s1,s2)