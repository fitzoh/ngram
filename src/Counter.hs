module Counter where
import qualified Data.Map as Map
import WordBuffer

--weights used for linear interpolation
data Weights = Weights {w3::Integer,
                        w2::Integer,
                        w1::Integer} |
                        Weight {w::Integer}
                        deriving Show 
  
                          
--all ngrams share these functions                              
class NgramCounter a where
        increment       :: a -> WordBuffer -> a
        add             :: a -> a -> a
        weight          :: a -> Weights -> a
        

--1-gram
type UniCount = Map.Map String Integer

uEmpt::UniCount
uEmpt = Map.empty

instance NgramCounter UniCount where
        increment uniCount wb =
                Map.insertWith (+) key 1 uniCount
                where
                        key = n_0 wb
                        
        add first second = Map.unionWith (+) first second
        
        weight uniCount weight = Map.mapWithKey (\_ x -> x*(w weight)) uniCount
                               
          
--2-gram         
type BiCount = Map.Map String UniCount

bEmpt::BiCount
bEmpt = Map.empty

instance NgramCounter BiCount where
        increment biCount wb =
                Map.insert key newValue biCount
                where
                        key = n_1 wb
                        uniCount = Map.findWithDefault Map.empty key biCount
                        newValue = increment uniCount wb
                        
        add first second = Map.unionWith add first second
        
        weight biCount w = Map.mapWithKey (\_ x -> weight x w) biCount        
                             
                               
--3-gram
type TriCount = Map.Map String BiCount

tEmpt::TriCount
tEmpt = Map.empty

instance NgramCounter TriCount where
        increment triCount wb =
                Map.insert key newValue triCount
                where
                key = n_2 wb
                biCount = Map.findWithDefault Map.empty key triCount
                newValue = increment biCount wb
                
        add first second = Map.unionWith add first second
        
        weight triCount w = Map.mapWithKey (\_ x -> weight x w) triCount


--combination of 1,2,3-gram            
data Counter = Counter {triC::TriCount, biC::BiCount, uniC::UniCount} deriving Show

eCount :: Counter
eCount = Counter tEmpt bEmpt uEmpt

instance NgramCounter Counter where
        increment count wb = 
                Counter t b u
                where
                        t = increment (triC count) wb
                        b = increment (biC count) wb
                        u = increment (uniC count) wb
                        
        add first second = 
                Counter t b u
                where
                        t = add (triC first) (triC second)
                        b = add (biC first) (biC second)
                        u = add (uniC first) (uniC second)
                        
        weight count weights = 
                Counter t b u
                where
                        t = weight (triC count) wt
                        b = weight (biC count) wb
                        u = weight (uniC count) wu
                        wt = Weight $ w3 weights
                        wb = Weight $ w2 weights
                        wu = Weight $ w1 weights
                    
--count a list of words (3,2,1-gram)    
countWords:: [String] -> Counter
countWords words =
            foldl (\count cb -> increment count cb) eCount cbList
            where
                    cbList = scanl (\cb word -> advanceWord cb word) eWB words
                    
--create prediction function using linear interpolation
linInterpolate:: Counter -> Weights -> Counter
linInterpolate  count  weights = Counter t b u
        where
                weighted = weight count weights
                t = Map.mapWithKey (\_ x -> add x b) (triC weighted)
                b = Map.mapWithKey (\_ x -> add x u) (biC weighted)
                u = uniC weighted
