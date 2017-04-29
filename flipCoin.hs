swap :: Char -> Char
swap x = if x=='H' then 'T' else 'H'

fliping :: String -> String
fliping [] = []
fliping (x:xs) = fliping xs ++ [swap x]

checkInput :: String -> String
checkInput x = [a|a<-x, a/='H' && a/='T']

checkEnd :: String -> Bool
checkEnd [] = True
checkEnd (x:xs) = if checkInput(x:xs) /= [] then error "invalid input"
	else
	if x=='T' then False else checkEnd xs

flipCoin :: String -> [Int]
flipCoin x = if checkEnd x then 
	[0]
	else
		if head x=='H' then 
			[1] ++ flipCoin ('T':tail x)
		else
			if last x == 'H' then 
				flipCoin (init x)
			else
				[length x] ++ flipCoin (fliping x)
