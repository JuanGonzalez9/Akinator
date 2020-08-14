module Main (main) where
  import Data.Char
  import Data.List
  import System.Exit
  import System.IO  
  import Oraculo
  import System.Directory
  import qualified Data.Map as Map

  main :: IO()
  main = do -- Comienza
    putStrLn "\nBienvenido al Akinator,"
    cicloAkinator $ crearOraculo ""

  -- Menu
  cicloAkinator :: Oraculo -> IO ()
  cicloAkinator oraculo = do
    putStrLn "\n¿Qué desea hacer?\n"

    putStrLn "1.- Cargar Conocimientos"
    putStrLn "2.- Predecir"
    putStrLn "3.- Salir"

    putStrLn "\nIngrese la opción:"
    opcion <- getLine

    case opcion of
      "1" -> do 
        nuevoOraculo <- cargar
        cicloAkinator nuevoOraculo
      "2" -> do
        nuevoOraculo <- (predecir oraculo)
        cicloAkinator nuevoOraculo
      "3" -> do
        putStrLn "\nAdios"
        return ()


  -- Recibe el Oráculo actual. Puede estar en una pregunta o en una predicción
  predecir :: Oraculo -> IO Oraculo
  predecir (Pregunta txt ops) = predecirPreg (Pregunta txt ops)
  predecir (Prediccion txt) = 
    if txt == "" then do
      putStrLn "\nError: No hay Conocimientos."
      return (Prediccion txt)
    else
      predecirPred (Prediccion txt)

  -- Predicción para Oráculo 
  predecirPreg :: Oraculo -> IO Oraculo
  predecirPreg oraculo = do
    putStrLn $ "Pregunta: " ++ pregunta oraculo
    imprimirOpciones $ opciones oraculo
    resp <- getLine
    case resp of
      "ninguna" -> do
        putStrLn "\n[!] ¡He fallado! Me retiro!"
        return(oraculo)
      _         -> 
        if resp `Map.member` (opciones oraculo) then do
          -- Se retorna la hoja con la respuesta elegida 
          sub <- predecir (respuesta oraculo resp)
          return (agregarOpcion oraculo sub resp)
        else do -- Respuesta no válida
          putStrLn "\nLa respuesta no es una opción válida. Intente de nuevo."
          predecirPreg oraculo

  -- Predicción para Oráculo de tipo Predicción.
  -- Retorna un Oráculo nuevo con los cambios correspondientes al 
  -- proceso de predicción
  predecirPred :: Oraculo -> IO Oraculo
  predecirPred oraculo = do
    putStrLn $ "Tu personaje es: " ++ prediccion oraculo
    putStrLn "si / no"
    resp <- getLine
    case resp of
      "si" -> do 
        putStrLn "\n[!] La predicción correcta!! \n Gané!!"
        return (oraculo)
      "no" -> do
        putStrLn "\n[!] Eso fue un error. Perdí\n"
        return (oraculo)

  agregarOpcion :: Oraculo -> Oraculo -> String -> Oraculo
  agregarOpcion (Pregunta txt ops) pred opc = Pregunta txt (Map.insert opc pred ops)

  -- Imprime las opciones a la pregunta y agrega la opcion "ninguna"
  imprimirOpciones :: Opciones -> IO ()
  imprimirOpciones ops = putStrLn $ (intercalate " / " (Map.keys ops)) ++ " / ninguna"


  -- Crea al oraculo con la informacion del archivo
  cargar :: IO Oraculo
  cargar = do
    putStrLn "Ingrese el nombre del archivo para cargar:"
    file <- getLine
    fileExists <- doesFileExist file
    if fileExists then do
      temp <- readFile file
      return (read temp :: Oraculo)
    else do
      putStrLn $ "Error: El archivo no existe."
      putStrLn "Intente de nuevo."
      cargar
