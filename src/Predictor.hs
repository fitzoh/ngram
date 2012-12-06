module Predictor where
import WordBuffer
import Data.List
import Data.Map as Map
import Data.Maybe
import Counter

--predictor based of 1-gram
type UniPredict = [(String,Integer)]

uniGet :: UniPredict ->WordBuffer -> Integer -> UniPredict
uniGet up wb count = take (fromIntegral count) predict
        where
                predict = Data.List.filter f up
                f = (\x -> isPrefixOf (n_0 wb) (fst x))
 
uniPredict :: UniCount -> UniPredict   
uniPredict uc  = sortBy cmp $ Map.toList uc
        where cmp = (\x y -> compare (snd y) (snd x))
     
--predictor based off 2-gram
type BiPredict = Map.Map String UniPredict

biGet :: BiPredict -> WordBuffer -> Integer -> UniPredict
biGet bp wb count = uniGet up wb count
        where 
                maybe = Map.lookup (n_1 wb) bp
                up = case maybe of
                        Just x -> x
                        Nothing ->[]
 
biPredict :: BiCount -> BiPredict
biPredict bc = Map.map (\x -> uniPredict x ) bc

--predictor based off 3-gram
type TriPredict = Map.Map String BiPredict

triGet :: TriPredict -> WordBuffer -> Integer -> UniPredict
triGet tp wb count = biGet bp wb count
        where 
                maybe = Map.lookup (n_2 wb) tp
                bp = case maybe of
                        Just x -> x
                        Nothing ->Map.empty
                
triPredict :: TriCount -> TriPredict
triPredict tc = Map.map (\x -> biPredict x ) tc


data Predictor = Predictor {triP :: TriPredict,
                            biP :: BiPredict,
                            uniP :: UniPredict} deriving Show
--predictor based off 3,2,1-gram
--falls back to smaller n-gram if no matches found in larger
predictor :: Counter -> Predictor
predictor c = Predictor t b u
        where
                t = triPredict $ triC c
                b = biPredict $ biC c
                u = uniPredict $ uniC c

predict :: Predictor -> WordBuffer ->Integer -> UniPredict
predict pred wb  count
        |tri /= [] = tri
        |bi /= [] = bi
        |otherwise = uni
        where
                tri = triGet (triP pred) wb count
                bi = biGet (biP pred) wb count
                uni = uniGet (uniP pred) wb count


   

