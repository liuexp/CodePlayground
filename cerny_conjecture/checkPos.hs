module Main where
import Data.List.Ordered(union,minus,nub)
import Data.List hiding(union,nub)
import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Set as DS
import qualified Data.Map as DM
import Data.Sequence(Seq,(><), (<|), (|>))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad
import Control.Applicative
import Data.Function
import Control.Parallel.Strategies

n=11
m=6

primMap' seed x=seed!!x
primMap seed xs=map (primMap' seed) xs

f0 x= nub $ sort $primMap ([1..n-1]++[0]) x 
f1 seed x = nub $sort $primMap seed x
isFinal = (==1).length .fst
applyMap op seed x
	|op == 0 = f0 x
	|otherwise = f1 seed x

checkAns seed init ans= scanl (\y x-> applyMap x seed y) init ans


atom = map (:[]) [0..m-1]
--generator = filter ((==m).length . group . sort) $ generator' n
generator = (++)<$> permutations [0..n-2] <*>(map (:[]) [0])
generator' k = if k==1 then atom else
		(++) <$> (generator' (k-1)) <*> atom


checkOne' seed (x,y)= [(f1 seed x,y++[1]), (f0 x,y++[0])]
checkOne seed =tail x++[head y] where
		(x,y) = maximumBy (compare `on` (length . fst)) $ map (span (not . any isFinal) . iterate (nubBy ((==) `on` fst) . sortBy (compare `on` fst) .(\x-> if length x >1 then (head x) else head x) .groupBy ((==) `on` (length. fst)) .sortBy (compare `on` (length. fst)) .concatMap (checkOne' seed) ))  [[([i,j],[])] | i<- [0..n-1], j<- [i+1..n-1] ]

solve = (map (length .checkOne) generator `using` parListChunk 5000 rdeepseq)
main = putStrLn $ show $ solve
	
