--Hecho por Jose Salazar--
--Fecha Inicio: 29/10/2020--
import Data.List
import System.IO
import Data.Char (isSpace)

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)


string2line :: String -> Line
string2line text = map (\palab -> Word palab) (words text)

line2string :: Line -> String
line2string linea = trim $foldl (++) "" (map convertTokenToString linea)
                    
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
lineLength linea = (length  $ words $ line2string linea) + sum (map tokenLength linea) - 1; 

breakLine :: Int -> Line -> (Line,Line)
breakLine m k = (k,k) 
                where
                
