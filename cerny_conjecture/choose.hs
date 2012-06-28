module Combi where
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

{-- non-memoized
choose n n = 1
choose n 0 = 1
choose n 1 = n
choose n k = choose (n-1) (k-1) + choose (n-1) k
--}
choose n k = pascalT !! n !!k where
		pascalT = iterate next [1] where 
			next xs = zipWith (+) (0:xs) (xs++[0])
{--
-- combinations of k-subset of xs
combinations k xs = (foldr f [[[]]] xs) !! k where 
	f x ys = zipWith (++) ([]:ysx) (ys++[[]]) where
		ysx=map (map (x:)) ys
--}
--

combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

