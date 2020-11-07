--Hecho por Jose Salazar--
--Fecha Inicio: 29/10/2020--
import Data.List
import Data.Char (isSpace)
import Control.Monad
import  Data.Map

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)

type HypMap = Data.Map.Map String [String]

enHyp = Data.Map.fromList [("controla",["con","tro","la"]),("futuro",["fu","tu","ro"]),("presente",["pre","sen","te"])]

string2line :: String -> Line
string2line text = Data.List.map (\palab -> Word palab) (words text)

line2string :: Line -> String
line2string linea = trim $Data.List.foldl (++) "" (Data.List.map convertTokenToString linea)
                    
convertTokenToString :: Token -> String
convertTokenToString (Word linea) = linea++" "
convertTokenToString (HypWord linea) = linea++"- "
convertTokenToString Blank = " "

trimLeft :: String -> String
trimLeft = dropWhile isSpace
 
trimRight :: String -> String
trimRight = dropWhileEnd isSpace
 
trim :: String -> String
trim = trimLeft . trimRight

tokenLength :: Token -> Int
tokenLength (Word token) = length token
tokenLength (HypWord token) = length token + 1    -- Tomar en cuenta el "-"
tokenLength (Blank) = 1

lineLength :: Line -> Int   
lineLength linea = (length  $ words $ line2string linea) + sum (Data.List.map tokenLength linea) - 1; 

breakLine :: Int -> Line -> (Line,Line)
breakLine _ [] = ([],[])
breakLine cont (x:xs) = if tokenLength x <= cont
                        then (x:m, j)  
                        else
                            ([], x:xs)
                        where y = cont - tokenLength x - 1
                              (m,j) = breakLine y xs

mergers :: [String] -> [(String,String)]
mergers  k = [ (join $fst $Data.List.splitAt x k, join $snd $Data.List.splitAt x k) | x <- [1.. (length k) -1] ] 


-- busca la palabra sin puntos en palabraArreglada luego signos le da la vuelta y agarra los signos y luego con
-- list comprehension saco de cada tupla los elementos y a la derecha le concateno los signos (variable)
hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate mapa token =  [ (HypWord $ fst y, Word $ snd y ++ signos) | y <- ys ]
                        where ys = mergers (rmvMaybeStr (Data.Map.lookup palabraArreglada mapa)) 
                              palabraArreglada = takeWhile (\ x -> x `notElem` ['.','!','?'] ) $ init $ convertTokenToString token
                              signos =  takeWhile (\ x -> x `elem` ['.','!','?']) $ reverse $ init $convertTokenToString token


rmvMaybeStr :: Maybe [String] -> [String]
rmvMaybeStr (Just linea) = linea
rmvMaybeStr Nothing = []
--enHyp = Data.Map.fromList [("controla",["con","tro","la"]),("futuro",["fu","tu","ro"]),("presente",["pre","sen","te"])]

lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
lineBreaks mapa cont linea = do
                            let (linea1, linea2) =  breakLine cont linea
                            (linea1,linea2): Data.List.map (\ (hyp,word)->  ((linea1++[hyp]), word:tail linea2))  (hyphenate mapa (Data.List.head linea2))

removeOverCount :: [(Line),(Line)] -> Int
removeOverCount lista = 