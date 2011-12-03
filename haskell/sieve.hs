genPrime' :: (Integral a) => [a] -> [a] -> [a]
genPrime' p [] = p
genPrime' p q = genPrime' (p++[x]) q'
		where x = head q
		      q' = filter (not.(==0).(flip mod x)) q

genPrime :: (Integral a) =>a -> [a]
genPrime n = genPrime' [2] [3,5..n]

main = interact (show . genPrime . read  . head . words. head . lines)
