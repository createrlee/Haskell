is_prime :: Int -> Bool
is_prime x = y == [1,x]
	where
	y = [a|a<-[1..x], mod x a == 0]

FindingPrimes :: Int -> Int -> [Int]
FindingPrimes x y = take y xl
	where
	xl = [a|a<-[x..], is_prime a]
