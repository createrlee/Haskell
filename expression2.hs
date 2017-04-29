interleave :: a -> [a] -> [[a]]  
interleave x [] = [[x]]  
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

permut :: String -> [String]  
permut [] = [[]]  
permut (x:xs) = concat (map (interleave x) (permut xs))

makeList :: Int -> Int -> String
makeList n d = if d/=n then ['A'] ++ ['B'] ++ makeList n (d+1) else []

makeAllPermut :: Int -> [String]
makeAllPermut n = permut (makeList n 0)

validString :: String -> Int -> Bool
validString [] 0 = True
validString (x:xs) n = if n<0 then False 
	else
		if x=='A' then validString xs (n+1)
		else validString xs (n-1)

isContain :: String -> [String] -> Bool
isContain xs xss = if k==l then True else False
	where 
	k = length [a|a<-xss, a/=xs]
	l = length xss

validStrings :: [String] -> [String]
validStrings [] = []
validStrings (xs:xss) = if validString xs 0 then 
		if isContain xs xss then [xs] ++ validStrings xss
		else validStrings xss 
	else validStrings xss  

makeX :: Int -> [String]
makeX n = validStrings (makeAllPermut n)


