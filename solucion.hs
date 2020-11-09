-- Hecho por Jose Salazar
-- Fecha Inicio: 29/10/2020

module Solucion where
import Data.List ( map, foldl, head, splitAt, tail, dropWhileEnd )
import Data.Char (isSpace)
import Control.Monad
import  Data.Map
import Data.Data (typeOf)

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)

data Argumento = NOAJUSTAR | AJUSTAR | NOSEPARAR | SEPARAR 
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

lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)] 
lineBreaks mapa cont linea = do
                            let (linea1, linea2) =  breakLine cont linea
                            if linea2 == []
                            then [(linea1,linea2)] 
                            else (linea1,linea2):  removeOverCount  (Data.List.map (\ (hyp,word)->  ((linea1++[hyp]), word:tail linea2))  (hyphenate mapa (Data.List.head linea2))) cont 

removeOverCount :: [(Line,Line)] -> Int -> [(Line,Line)]
removeOverCount [] _ = []
removeOverCount lista cont = if lineLength (fst $ Data.List.head lista) > cont
                             then []
                             else
                                head lista : removeOverCount  (Data.List.tail lista)  cont 

insertBlanks :: Int -> Line -> Line
insertBlanks _ [] = []
insertBlanks _ [xs] = [xs]
insertBlanks cont linea = do
                          let cantidadAsegurada = quot cont $ (length linea -1)
                          let faltantes = cont - (cantidadAsegurada * (length linea -1))
                          if length linea <= cont
                          then
                            addSomeBlanks faltantes $reverse$ dropWhile (\x -> x==Blank) $ reverse (concat $ Data.List.map (\ x -> x:concat (replicate cantidadAsegurada [Blank]) )  linea)
                          else
                            addSomeBlanks cont linea


addSomeBlanks :: Int -> Line -> Line
addSomeBlanks _ [] = []
addSomeBlanks 0 xs = xs 
addSomeBlanks cont (x:xs) = if x/=Blank
                            then [x] ++ [Blank] ++ addSomeBlanks (cont-1) xs
                            else [x] ++ addSomeBlanks cont xs 


separarYalinear :: Int -> Argumento -> Argumento -> String -> [String]
separarYalinear cont SEPARAR AJUSTAR  frase = ajustarSeparar cont frase 
separarYalinear cont NOSEPARAR AJUSTAR  frase = ajustarNoSeparar cont frase
separarYalinear cont SEPARAR NOAJUSTAR  frase = noAjustarSeparar cont frase
separarYalinear cont NOSEPARAR NOAJUSTAR frase = noAjustarNoSeparar cont frase


ajustarSeparar :: Int -> String -> [String]
ajustarSeparar cont lineaLeida = Data.List.map (\x -> (line2string x) ) $ ajustarSepararAux enHyp cont (string2line lineaLeida)

ajustarSepararAux ::HypMap -> Int -> Line -> [Line] 
ajustarSepararAux _ _ [] = []
ajustarSepararAux enHyp cont linea = do 
                                    let espacio = cont - (lineLength $fst ( last (lineBreaks  enHyp cont linea)))
                                    if snd ( last (lineBreaks  enHyp cont linea)) /= []
                                    then [ insertBlanks espacio $fst ( last (lineBreaks  enHyp cont linea))] ++ ajustarSepararAux enHyp cont (snd (last (lineBreaks  enHyp cont linea)))
                                    else [fst ( last (lineBreaks  enHyp cont linea))] ++ ajustarSepararAux enHyp cont (snd (last (lineBreaks  enHyp cont linea)))

noAjustarSeparar :: Int -> String -> [String] 
noAjustarSeparar cont lineaLeida = Data.List.map (\x -> (line2string x) ) $ noAjustarSepararAux enHyp cont (string2line lineaLeida)

noAjustarSepararAux :: HypMap -> Int -> Line -> [Line] 
noAjustarSepararAux _ _ [] = []
noAjustarSepararAux enHyp cont linea = [fst ( last (lineBreaks  enHyp cont linea))] ++ noAjustarSepararAux enHyp cont (snd (last (lineBreaks  enHyp cont linea))) 


--Penultimo CASO
ajustarNoSeparar :: Int -> String -> [String]
ajustarNoSeparar cont lineaLeida = Data.List.map (\x -> (line2string x) ) $ separarLineBreaksBlank enHyp cont (string2line lineaLeida)

separarLineBreaksBlank :: HypMap -> Int -> Line -> [Line]
separarLineBreaksBlank _ _ [] = []
separarLineBreaksBlank enHyp cont linea = do 
                                        let espacio = cont - (lineLength $fst ( head (lineBreaks  enHyp cont linea)))
                                        if snd ( head (lineBreaks  enHyp cont linea)) /= []
                                        then [ insertBlanks espacio $fst ( head (lineBreaks  enHyp cont linea))] ++ separarLineBreaksBlank enHyp cont (snd (head (lineBreaks  enHyp cont linea)))
                                        else [fst ( head (lineBreaks  enHyp cont linea))] ++ separarLineBreaksBlank enHyp cont (snd (head (lineBreaks  enHyp cont linea)))
--ULTIMO CASO
noAjustarNoSeparar :: Int -> String -> [String]
noAjustarNoSeparar cont lineaLeida = Data.List.map (\x -> (line2string x) ) $ separarLineBreaks enHyp cont (string2line lineaLeida)

separarLineBreaks :: HypMap -> Int -> Line -> [Line]
separarLineBreaks _ _ [] = []
separarLineBreaks enHyp cont linea = [fst ( head (lineBreaks  enHyp cont linea))] ++ separarLineBreaks enHyp cont (snd (head (lineBreaks  enHyp cont linea)))
