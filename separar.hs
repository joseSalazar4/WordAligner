f :: Int -> [a] -> ([a],[a])
f _ [] = ([],[])
f 0 (x:xs) = ([],x:xs)
f n (x:xs) = let (li1,li2) = f (n-1) xs
               in (x:li1,li2)

separe :: Int -> [a] -> [[a]]
separe _ [] = []
--separe n li = let (li1,li2) =  f n li
--                  li3 = separe n li2
--                in (li1:li3)

separe n li = let (li1,li2) =  f n li
                in (li1:separe n li2)

g :: (Int,Int,[[a]],[a]) -> a -> (Int,Int,[[a]],[a])
g (n,c,lig,lit) x = if c<n then (n,c+1,lig,lit++[x])
                           else (n,1,lig++[lit],[x])
separe2 :: Int -> [a] -> [[a]]
separe2 _ [] = []
separe2 n (x:xs) = let
                      (_,_,lig,lit) = foldl (g) (n,1,[],[x]) xs
                    in lig ++ [lit]

