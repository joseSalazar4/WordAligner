module Conteo where

import Prelude hiding (null, lookup, map, filter)  
import Solucion( separarYalinear, HypMap, Argumento(SEPARAR,AJUSTAR,NOSEPARAR,NOAJUSTAR))
import qualified Data.Map as Map
import System.IO
import Data.List (intercalate )
import Data.Char

type Estado = HypMap

main :: IO ()
main = do
  mainloop (Map.fromList [])

-- Corrida principal 
mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens = words inpStr
  let comando = tokens !! 0
  case comando of

    "load" -> do
      let nombreArchivo = (tokens !! 1)
      inh <- openFile nombreArchivo ReadMode
      nuevoEstado <- loadDiccionario inh estado
      hClose inh
      putStrLn (("Diccionario cargado ") ++ (show (length (Map.keys nuevoEstado))) ++ " palabras cargadas")
      mainloop nuevoEstado

    "show" -> do
      putStrLn (show estado)
      mainloop estado

    "ins" -> do 
      let nuevaPalab = (tokens !! 1)
      let listaSilab = (wordsWhen (=='-') $ tokens !! 2) 
      let estadoActualizado = incluirToken estado nuevaPalab listaSilab 
      mainloop estadoActualizado

    "save" -> do 
      putStrLn ">>> Nombre archivo salida: "
      nombreArchivo <- getLine
      outh <- openFile nombreArchivo WriteMode
      descargar outh $ Map.toList  estado  
      hClose outh
      mainloop estado

    "split" -> do 
        let longitud = tokens !! 1
        let separar = tokens !! 2
        let ajustar = tokens !! 3
        let texto = intercalate " " $ drop 4 tokens
        putStrLn $  intercalate "\n" $splitCreator (read longitud :: Int )  separar ajustar estado texto
        mainloop estado

    "splitf" -> do 
        let longitud = tokens !! 1
        let separar = tokens !! 2
        let ajustar = tokens !! 3
        let arch1 = tokens !! 4      
        let arch2 = tokens !! 5  

        inh <- openFile arch1 ReadMode
        texto <- hGetContents inh
        let contenido = intercalate "\n" $splitCreator (read longitud :: Int )  separar ajustar estado texto
        

        if length tokens == 6
        then do
          outh <- openFile arch2 WriteMode
          hPutStr outh contenido  
          hClose outh
        else 
          putStrLn $  intercalate "\n" $splitCreator (read longitud :: Int )  separar ajustar estado texto
        hClose inh
        mainloop estado

    "exit" -> do
      putStrLn "Saliendo..."
    _ -> do
      putStrLn $ "Comando desconocido (" ++ comando ++ "): '" ++ inpStr ++ "'"
      mainloop estado


incluirToken :: Estado -> String -> [String] -> Estado
incluirToken estado token silabas =
  if Map.member token estado
    then estado
    else Map.insert token silabas estado

loadDiccionario :: Handle -> Estado -> IO Estado
loadDiccionario inh estado = do
  ineof <- hIsEOF inh
  if ineof
    then return estado
    else do
      inpStr <- hGetLine inh
      let linea = words (inpStr)
      let silab = linea !! 1
      let newHash = incluirToken estado (head linea) (words [if c == ',' then ' ' else c | c <- silab])
      loadDiccionario inh newHash


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s' 

descargar _ [] = return ()
descargar outh ((k,v):kvs) = do hPutStrLn outh $ k ++ " " ++ (intercalate "-" v)
                                descargar outh kvs

descargar2 :: Handle -> String -> IO ()
descargar2 outh linea = do hPutStrLn outh linea 

--splitCreator :: 
splitCreator :: Int -> String -> String -> HypMap -> String -> [String]
splitCreator longitud separa ajusta estado texto  | (separa == "s") && (ajusta == "s") = separarYalinear  longitud SEPARAR   AJUSTAR estado texto    
                                                  | (separa == "s") && (ajusta == "n") = separarYalinear  longitud SEPARAR   NOAJUSTAR  estado texto
                                                  | (separa == "n") && (ajusta == "s") = separarYalinear  longitud NOSEPARAR AJUSTAR  estado texto
                                                  | (separa == "n") && (ajusta == "n") = separarYalinear  longitud NOSEPARAR NOAJUSTAR  estado texto
                                                  | otherwise = ["No llegue a ningun caso"]
