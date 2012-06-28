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

type Config = [Int]
-- here a configuration is just the set of positions present.

n=11
m=6

primMap' seed x=seed!!x
primMap seed xs=map (primMap' seed) xs

f0 x= nub $ sort $primMap ([1..n-1]++[0]) x 
f1 seed x = nub $sort $primMap seed x
isFinal = (==1).length .fst

atom = map (:[]) [0..m-1]
--generator = filter ((==m).length . group . sort) $ generator' n
generator = (++)<$> permutations [0..n-2] <*>(map (:[]) [0])
generator' k = if k==1 then atom else
		(++) <$> (generator' (k-1)) <*> atom


{--
delta (x:y:xs) = y-x : delta (y:xs)
delta xs = []

-- TODO: maybe use Data.Foldable and Sequence to improve this to O(n) rather than O(n^2)
getDistSet seed = DS.fromList $filter (>0) $ getDist <$> y<*> y
			where y = zip seed [1..]
			      getDist (x1,z1) (x2,z2)
			      		|x1==x2 =z2-z1
				      |otherwise = 0
			    
--getCurDistSet = DS.fromList .delta . nub. sort
getCurDistSet x= DS.fromList $ filter (>0) $pure (-) <*> x <*> x

--get candidate positions for applying f1
getCandPos:: [Int] -> [[(Int,Int)]]
getCandPos =filter ((>1).length) .groupBy ((==) `on` snd) .sortBy ((compare) `on` snd) .zip [0..]

getIsoPos=minus [0..n-1] .nub . sort


--TODO:maybe change `elem` to be something more efficient like DS.member?
isReducible seed x = any ((all (`elem` x) ).(map fst)) $getCandPos seed

isAdjustable seed x =any (`elem` x) $getIsoPos seed

maybeReducible seed x = not $ DS.null $ DS.intersection (getDistSet seed) (getCurDistSet x)

solveOne' :: [Int] -> ([Int],[Int])->([Int],[Int])
solveOne' seed xx
	|isReducible seed x = y1
	|isAdjustable seed x && fst y1/= x && (not $maybeReducible seed x)= y1
	|maybeReducible seed x = y0
	|otherwise = y1
	where x=fst xx
	      y=snd xx
	      y1=(f1 seed x,y++[1])
	      y0=(f0 x,y++[0])

solveOne seed = takeWhile (not . isFinal) $iterate (solveOne' seed ) ([0..n-1],[])
--solve = map solveOne generator
--}
solveOne2' seed (x,y)= [(f1 seed x,y++[1]), (f0 x,y++[0])]
solveOne2 seed =tail x++[head y] where
		(x,y) = span (not . any isFinal) $ iterate (nubBy ((==) `on` fst) . sortBy (compare `on` fst) .(\x-> if length x >1 then (head x) else head x) .groupBy ((==) `on` (length. fst)) .sortBy (compare `on` (length. fst)) .concatMap (solveOne2' seed) ) [([0..n-1],[])]
	
solve = take 2 $filter (>(n-1)^2) (map (length .solveOne2) generator `using` parListChunk 5000 rdeepseq)
main = putStrLn $ show $ solve
