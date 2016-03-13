import MusicResources
import Data.List
makeStatsList :: [(Char,[(Int,Char)])]
makeStatsList = makeStatsHelp	chars flatTrain

flatTrain = flatten training

flatten [] =[]
flatten (x:xs) = x ++ ['A'] ++ flatten xs

occurrences ::  [Char] -> String -> Int -> Int
occurrences [x,y] "" count = count
occurrences [x,y] [u] count = count 
occurrences [x,y] (w:z:xs) count | x == w && y == z = occurrences [x,y] (z:xs) (count+1)
								 | otherwise = occurrences [x,y] (z:xs) count
								 
makeStatsHelp [] ft = []
makeStatsHelp (x:xs) ft	| elem x ft = (x, sort2 (occMaker x chars)):makeStatsHelp xs ft
						| otherwise = (x,[]):makeStatsHelp xs ft

occMaker x [] = []
occMaker x (y:ys)	| occurrences [x,y] flatTrain 0 == 0 = occMaker x ys
					| otherwise = [(occurrences [x,y] flatTrain 0,y)] ++ occMaker x ys
					
sort2 l = reverse (sort l)
------------------------------------------------------------------------------------------------								 
composeHelperSum []  = 0
composeHelperSum ((x,y):xs) = x + composeHelperSum xs 
--try with: composeHelperSum [(7,'d'),(5,'u'),(5,'s'),(4,'p'),(3,'y'),(2,'w'),(1,'O'),(1,'q'),(1,'o'),(1,'0')]
--composeGetLetter [] n = error "The letter is not in the list"
--composeGetLetter ((x,y):xs) n = composeGetLetterHelper ((x,y):xs) n --s where s = composeHelperSum ((x,y):xs)
composeGetLetter [] n = error "The letter is not in the list"
composeGetLetter ((x,y):xs) n  | n > x = composeGetLetter xs (n-x) 
									  | otherwise = y
--try with: composeGetLetter [(7,'d'),(5,'u'),(5,'s'),(4,'p'),(3,'y'),(2,'w'),(1,'O'),(1,'q'),(1,'o'),(1,'0')] 8
compose a 0 =[]
compose a n = a:compose y (n-1) where y = composeGetLetter (getList a makeStatsList) (randomZeroToX (composeHelperSum (getList a makeStatsList)))
				
getList c ((f,h):xs) | f == c = h
					 | otherwise = getList c xs