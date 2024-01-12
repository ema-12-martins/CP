import System.Random
import Probability
import Data.List

exp1 :: Dist Char
exp1 = D [('A',0.02),('B',0.12),('C',0.29),('D',0.35),('E',0.22)]

exp2 = Probability.uniform (words "Uma frase de cinco palavras")

exp3 = Probability.normal [10..20]

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

--Ausencia de qualquer atraso
instantaneous :: Dist Delay 
instantaneous = D [(0,1)]

---------- Exercicio propriamente dito -------------------

-- Faz sumario estatisticas e qualquer lista finita, gerando a distribuicao dos seus elementos

groupTuples :: Eq a => [(a, b)] -> [[(a, b)]]
groupTuples = groupBy (\(x, _) (y, _) -> x == y)

mkdist :: Eq a => [(a, Delay)] -> [(a, Float)]
mkdist [] = []
mkdist l = [(fst (head l), probabilidade)]
         where atraso_maior_que_zero = length (filter (\(_, t) -> t > 0) l)
               total_elementos = length l
               probabilidade = fromIntegral atraso_maior_que_zero / fromIntegral total_elementos

probabilidadePares :: Eq a => [(a, Float)] -> [(a, Float)]
probabilidadePares lista = map (\(x, y) -> (x, y / total)) lista
  where
    total = sum (map snd lista)

tentativa_probabilidades :: Eq a => [(a, Delay)] -> [(a, Float)]
tentativa_probabilidades [] = []
tentativa_probabilidades l = probabilidadePares (concatMap mkdist (groupTuples l))


--Gerar base de dados de probabilidades
{- b :: [(Segment,Dist Delay)] -}

-- Com a bd, definir funcao de probabilidade
{- delay :: Segment -> Dist Delay -}




--Dar o delay acomulado do percurso
{- pdelay :: Stop -> Stop -> Dist Delay
pedelay x x = return 0
pedelay x y = do{
    x <- delay;
    y <- pedelay 
} -}




