import Data.Char

findMines :: Int -> Int -> Int-> String -> String
findMines m n d x  = if d==m*n then 
		[]
	else
		if (x!!d)=='*' then
			if d/=0 && mod d n == 0 then
				['/'] ++ ['*'] ++ findMines m n (d+1) x
			else 
				['*'] ++ findMines m n (d+1) x
		else
			if d/=0 && mod d n == 0 then
				['/'] ++ [k] ++ findMines m n (d+1) x
			else
				[k] ++ findMines m n (d+1) x
			where
			p = if mod d n == 0 then
				[d+1,d-n,d-n+1,d+n,d+n+1]
			else if mod d n == (n-1) then
				[d-1,d-n,d-n-1,d+n,d+n-1]
			else
				[d+1,d-1,d+n,d+n-1,d+n+1,d-n,d-n+1,d-n-1]
			q = [a | a<-p, a<m*n, a>=0]
			r = [b | b<-q, (x!!b)=='*']
			k = intToDigit (length r)

checkInput :: String -> Bool
checkInput [] = True
checkInput (x:xs) = if x=='.' || x=='*' || x=='/' then checkInput xs
	else False

checkCount :: Int -> Int -> Int -> Int -> String -> Bool
checkCount m n d e [] = if e==(m-1) then True else False
checkCount m n d e (x:xs) = if x=='/' then 
		if d==n	then checkCount m n 0 (e+1) xs
		else False
	else if d<n then checkCount m n (d+1) e xs else False

minesweep :: Int -> Int -> String -> String
minesweep x y s = if checkInput s && checkCount x y 0 0 s then findMines x y 0 k else error "invalid input"
	where
	k = [a|a<-s, a/='/']
