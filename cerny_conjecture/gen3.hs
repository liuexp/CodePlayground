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
import System.IO (interact)

type Config = [Int]
-- here a configuration is just the set of positions present.


primMap' seed x=seed!!x
primMap seed xs=map (primMap' seed) xs

f0 seed (x,y)= (nub $ sort $primMap ([1..n-1]++[0]) x ,0:y) where n = length seed
f1 seed (x,y) = (nub $sort $primMap seed x,1:y)

--invMap seed = 
hasExtended  = ((<) `on` (length .fst ))
isDone n x = (==n) $length$ fst x
f1inv seed (x,y) = (foldl1 union $map (f1inv' seed) x,1:y) where
		f1inv' seed y = sort $ map snd $filter ((==y).fst) $zip seed [0..]

solveOne' seed c@(x,y) =head $ dropWhile (not .(hasExtended c)) $map (f1inv seed) $iterate (f0 seed) c
solveOne seed start = head $ dropWhile (not.(isDone (length seed))) $ iterate (solveOne' seed) start

n=11
m=6
atom = map (:[]) [0..m-1]
--generator = filter ((==m).length . group . sort) $ generator' n
generator = (++)<$> permutations [0..n-2] <*>(map (:[]) [0])
generator' k = if k==1 then atom else
		(++) <$> (generator' (k-1)) <*> atom
	

-- automatic test
check = head $filter (>(n-1)^2) (map (length .snd .solve ) generator `using` parListChunk 5000 rdeepseq)
main = putStrLn $ show $ check


-- interactive environment
solve seed = (\y -> (length y, y)) $minimumBy (compare `on` (length )) $ map (snd .solveOne seed) [([i],[])|i<-[0..n-1]] where n= length seed
--main = interact (show .solve . read)
