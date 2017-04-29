calMini :: Int->Int->Int
calMini n d = if n < 0 || d < 0 then 0
 	else if n == 0 && d >= 0 then 1
 	else calSum n d 2

calSum :: Int->Int->Int->Int
calSum n d k = if n == k then (calMini (k-2) (d-1) ) * (calMini (n-k) d )
	else (calMini  (k-2) (d-1) ) * (calMini (n-k) d ) + calSum n d (k+2)

expression ::Int->Int->Int
expression n d = if mod n 2 /= 0 || n<2 || d<1 then error "invalid input"
 	else (calMini n d) - (calMini n (d-1) )
