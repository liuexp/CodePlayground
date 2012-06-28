module Gen where
import Data.List.Ordered(union,minus)
import Data.List hiding(union)
import Data.Set(Set)
import Data.Map(Map)
import qualified Data.Set as DS
import qualified Data.Map as DM
import Data.Sequence(Seq,(><), (<|), (|>))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad
import Control.Applicative


choose n k = pascalT !! n !!k where
		pascalT = iterate next [1] where 
			next xs = zipWith (+) (0:xs) (xs++[0])


combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

n=7
m=6
atom = map (:[]) [0..m-1]
generator = filter ((==m).length . group . sort) $generator' n
generator' k = if k==1 then atom else
		(++) <$> (generator' (k-1)) <*> atom

primMap' seed x=seed!!x
primMap seed xs=map (primMap' seed) xs

f0 x= primMap ([1..n-1]++[0]) x 
genImageSet seed xs = sort$ nub $primMap seed xs

genValidSet seed  = DS.fromList $ delta $genImageSet seed [0..n-1]

delta (x:y:xs) = y-x : delta (y:xs)
delta xs = []

