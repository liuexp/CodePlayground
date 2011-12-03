module Main where
import Data.List
import Data.List.Split
import Text.Printf (printf)
import Control.Applicative
{--
mkDouble a b = a+b/60.0

mkList x y = zipWith mkDouble x y
 
mkAngle x = (toInteger (floor x), floor $ (x - fromInteger ( floor x))* 60)

raw01= mkDouble 182 40
raw02= mkDouble 2 39
raw11=mkList [196,189,175,168] [40,40,39,45]
raw12 = mkList [16,9,355,348] [ 41,40,40,44]
--}
--
eps = 1e-5
split2 f x= (head y,head $tail y)
		where y = splitWhen (=='.') x

groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)  

--average xs = realToFrac (sum xs) / genericLength xs
average xs = (sum xs) /( genericLength xs)

--processRaw file = readFile file >>= return . ( map words).(filter (not. null) ) .lines >>= return .process
processRaw file = readFile file >>= return . ( map words).(filter (not. null) ) .lines>>= return.(mapTable ( convRaw))
convRaw x = mkAngle $split2 (=='.') x

--intPart :: (RealFrac a) => a-> Integer
intPart x = if x >0 then toInteger (floor (x+eps))  else toInteger (floor (x-eps)) +1

--floatPart :: (RealFrac a) => a ->a
floatPart x = x - (fromInteger $intPart x)
--floatPart' :: (RealFrac a) => a -> a
floatPart' x = abs(floatPart x)

mapTable f x = map (map f) x
formatTable f x = join' "\t \\tabularnewline  \\hline \n" $ map ((join' " & "). (map f)) x
		where	join' a [] = "\\multicolumn{<++>}{|c|}{<++>}" 
			join' a b = concat $ intersperse a b


theta01= 	mkAngle' (182,40)
theta02= 	mkAngle' (2,39)
theta11=	mkAngle' (182,45)
theta12 = 	mkAngle' (2,45)

-- particularly for delta angle
normAngle y =  if y> pi then y-2*pi
			 else if y< -pi then y+2*pi
			 else y

mkAngle' (a,b) = ( a + b /60.0)*pi/180.0
mkAngle (a,b) = y --normAngle y
		where a' = read a
		      b' = if a' >0 then read b else (-(read b))
		      y= (a' + b' /60.0)*pi/180.0

showAngle x = if (x<2*pi) &&  (x > -2*pi) then "$"++show (intPart x')++ "^{\\circ}"++ (show (intPart (60.0*(floatPart' x')))) ++ "\'$"
				      else printf "%.2f" (x:: Float)
		where x' = if x >0 then x*180.0/pi +eps else x*180.0/pi -eps

process file =  processRaw file  >>=return. ( partition ((==4).length))

listK=[-2,-1,1,2]
listK2=[-2,2]
avgD= return . average.(!!5) =<< table1' 
avgD'=3332.22

table1 = return .transpose =<< table1'
table1'= processRaw "data1" >>= return . (appendBy getD (theta01,theta02))
	where getD avg =map getD' $ zip listK avg
	      getD' (k,th) = k*lambda/(sin th)
	      lambda =546.1

table2 = process "data">>=return .concat. (intersperse [[]]). ( map ((\x -> (transpose x ++[[(average (x!!5))]])). ( appendBy  getL (theta01,theta02)))).( groupsOf 2)  . fst 
	where getL avg = map getL'  (zip listK avg)
	      getL' (k,th) = avgD'* sin(th) / k
	--where getL avg = (\avgD' -> map (getL' avgD')  (zip listK avg)) =<<avgD
	      --getL' avgD' (k,th) = avgD'* sin(th) / k
	

table3 = process "data">>=return .concat. (intersperse [[]]). ( map ((\x -> (transpose x ++[[(average (x!!5))]])).( appendBy  getL (theta11,theta12)))).( groupsOf 2)  . snd 
	where getL avg = map getL'  (zip listK2 avg)
	      getL' (k,th) = avgD'* sin(th) / k

--appendBy f [x@[x0,x1,x2,x3],y@[y0,y1,y2,y3]] (a,b) = [x,dx,y,dy,avg,favg]
appendBy f  (a,b) [x,y]= [x,dx,y,dy,avg,favg]
			where dx = map (normAngle .((-) a)) x
			      dy = map (normAngle .((-) b)) y
			      avg = map (*0.5) $zipWith (+) dx dy
			      favg = f avg 

--main = putStrLn $ "abc"   ++  show ( map (flip (-) raw01) raw11)  --show (mkAngle 2.5) ++ show (mkDouble 2 30)
--main = processRaw "data" 
--main = putStrLn $show $convRaw "-60.30"
--main = putStrLn $show $map (showAngle. convRaw) ["60.30","-60.30","60.0","-60.0","202.50"]

--main = processRaw "data">>= putStrLn . (formatTable (show . convRaw)). fst 
--main = processRaw "data">>= putStrLn.(formatTable show) . (mapTable ( convRaw)). fst 
--main = process "data">>= putStrLn . (formatTable showAngle). concat.( map ( appendBy  id (theta01,theta02))).( groupsOf 2). fst 

main = putStrLn . (formatTable showAngle) =<< ((++) <$> ((++ [[]]) <$> table1) <*> ((++) <$> ((++[[]]) <$> table2 )<*> table3))
--main = putStrLn .show  =<< ((++) <$> ((++ [[]]) <$> table1) <*> ((++) <$> ((++[[]]) <$> table2 )<*> table3))
--main = avgD
