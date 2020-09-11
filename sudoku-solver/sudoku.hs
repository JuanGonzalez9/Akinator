import Data.List
import Control.Exception
import System.Environment  
import System.IO  
import System.IO.Error

type Sudoku = [[Int]]
type Options = [[Int]]

printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . unlines . map (unwords . map show)

opt :: Options
opt = [[1,3,4,5],[2,3,4],[3],[4]]

opt2 :: Options
opt2 = [[1,3,4,5],[2,3,4],[6,1],[5,1]]

opt3 :: Options 
opt3 = [[1,4,8,9],[1,5,8,9],[3],[2],[1,6,8],[7],[9],[1,5,6],[1,5,6]]

s1 :: Sudoku
s1 = [[0,0,0,2,6,0,7,0,1],[6,8,0,0,7,0,0,9,0],[1,9,0,0,0,4,5,0,0],
       [8,2,0,1,0,0,0,4,0],[0,0,4,6,0,2,9,0,0],[0,5,0,0,0,3,0,2,8],
       [0,0,9,3,0,0,0,7,4],[0,4,0,0,5,0,0,3,6],[7,0,3,0,1,8,0,0,0]]

s2 :: Sudoku
s2 = [[5,3,4,6,7,8,9,1,2],[6,7,2,1,9,5,3,4,8],[1,9,8,3,4,2,5,6,7],
       [8,5,9,7,6,1,4,2,3],[4,2,6,8,5,3,7,9,1],[7,1,3,9,2,4,8,5,6],
       [9,6,1,5,3,7,2,8,4],[2,8,7,4,1,9,6,3,5],[3,4,5,2,8,6,1,7,9]]

s3 :: Sudoku
s3 = [[0,0,0,0,0,2,0,3,0],[3,4,8,0,0,6,0,7,0],[6,0,7,0,3,0,5,0,4],
      [0,0,3,2,0,0,0,0,0],[2,0,0,4,0,5,8,0,0],[0,6,0,9,0,0,2,4,0],
      [0,0,4,0,0,9,3,2,0],[0,0,0,3,0,0,7,0,0],[5,0,2,0,0,8,0,0,0]]

rowIsValid :: [Int] -> Bool
rowIsValid [] = True
rowIsValid (x : xs)
    -- si hay un cero no es valido, 
    -- si hay repetidos no es valido
    -- otherwise se fija si es valida la cola
    | x == 0 || elem 0 xs = False
    | elem x xs           = False
    | otherwise           = rowIsValid xs


rowsAreValid :: Sudoku -> Bool
rowsAreValid [] = True
-- es valido si es valida la cabeza y la cola
rowsAreValid (x : xs) = rowIsValid x && rowsAreValid xs

colsAreValid :: Sudoku -> Bool
-- para ver si las columnas son validas, transpongo la matriz y llamo a la funcion de filas
colsAreValid s = rowsAreValid ( transpose s )


createSquare :: Int -> Int -> Sudoku -> [Int]
-- crea una lista de enteros, con todos los numeros de un cuadrado
-- n -> numero de veces q se llama a esta funcion. Se llama 3 veces siempre
-- m -> numero de Cuadrado que quiero crear ( puede ser 0,1,2)
-- (x:xs) -> el sudoku
createSquare 0 _ _ = []
createSquare n m (x : xs) = take 3 (drop (3 * m) x) ++ createSquare (n-1) m xs

createSquareOpt :: Int -> Int -> [Options] -> Options
-- lo mismo pero en vez de pasarle el Sudoku (solo numeros) le pasa las opciones
createSquareOpt 0 _ _ = []
createSquareOpt n m (x : xs) = take 3 (drop (3 * m) x) ++ createSquareOpt (n-1) m xs

createSquares :: Int -> Sudoku -> [[Int]]
-- crea una lista de listas, con los 3 primeros cuadrados, o los siguientes 3, o los ultimos 3
-- row puede ser 0, 1 o 2
createSquares row xs = [createSquare 3 0 x,createSquare 3 1 x,createSquare 3 2 x]
    where x = drop (3*row) xs

createSquaresOpt :: Int -> [Options] -> [Options]
-- Lo mismo que createSquares pero con Options
createSquaresOpt row xs = [createSquareOpt 3 0 x,createSquareOpt 3 1 x,createSquareOpt 3 2 x]
    where x = drop (3*row) xs

createAllSquares :: Sudoku -> Sudoku
-- crea una copia del sudoku, donde cada fila es uno de los cuadrados
createAllSquares xs = createSquares 0 xs ++ createSquares 1 xs ++ createSquares 2 xs

createAllSquaresOpt :: [Options] -> [Options]
-- lo mismo que createAllSquares pero con options 
createAllSquaresOpt xs = createSquaresOpt 0 xs ++ createSquaresOpt 1 xs ++ createSquaresOpt 2 xs

squaresAreValid :: Sudoku -> Bool
squaresAreValid xs = rowsAreValid $ createAllSquares xs

isValid :: Sudoku -> Bool
isValid s = rowsAreValid s && colsAreValid s && squaresAreValid s

getAlreadyTakenValues :: Options -> [Int]
-- devuelve los valores que ya están fijos
getAlreadyTakenValues [] = []
getAlreadyTakenValues (x:xs)
    | length x == 1 = x ++ getAlreadyTakenValues xs
    | length x > 1 = getAlreadyTakenValues xs

cleanCell :: [Int] -> [Int] -> [Int]
-- x es la lista de numeros a eliminar. x es la lista de los posibles numeros de una celda
cleanCell x y
    | length y == 1 = y
    | length y > 1  = y \\ x 

cleanRows :: Options -> Options
-- Recibe una fila, cada elemento tiene todos los valores que puede tomar la celda
-- La funcion se fija todas las celdas con 1 sola opcion, y elimina esa opcion de las otras celdas
-- Ej: Si tengo un 8 al principio de una fila, entonces ningun otra celda de esa fila puede tener un 8
cleanRows x = map (cleanCell (getAlreadyTakenValues x)) x

cleanOptions :: [Options] -> [Options]
cleanOptions opt = createAllSquaresOpt (map cleanRows (createAllSquaresOpt (transpose ( map cleanRows (transpose (map cleanRows opt))))))

getOptions :: Int -> [Int]
getOptions n
    | n == 0 = [1..9]
    | n > 0  = [n]

generateOption :: [Int] -> Options
generateOption [] = []
-- Recibe una fila del Sudoku y devuelve el mismo, pero en vez de 0's aparecen todos los numeros  
-- posibles que puede tomar esa celda (de 1 a 9)
generateOption xs = map getOptions xs

generateOptions :: Sudoku -> [Options]
-- generateOption pero para todas las filas
generateOptions xs = map generateOption xs

getTotalOptions :: [Options] -> Int
-- Me da la cantidad total de opciones que hay en el sudoku. En un sudoku resuelto, devuelve 81 (una opcion en cada cacillero)
getTotalOptions [] = 0
getTotalOptions (x : xs) = sum (map length x) + getTotalOptions xs

countPossibilities :: Options -> Int -> Int
-- cuenta la cantidad de cacilleros en los que podria ir un número
countPossibilities [] _ = 0
countPossibilities (x : xs) n = if (elem n x) then 1 + (countPossibilities xs n) else countPossibilities xs n

selectTheOnlyOption :: Options -> Int -> Options
-- cuando un numero puede ir en un solo cacillero de la fila, elije ese cacillero
-- ej: [[1,2,3],[2,3],[2,3]] en este caso, el 1 solo puede estar en el primer cacillero, entonces devuelve [[1],[2,3],[2,3]]
selectTheOnlyOption [] _ = []
selectTheOnlyOption (x : xs) n = if (elem n x) then [n] : selectTheOnlyOption xs n else x : selectTheOnlyOption xs n

selectSingleOptions :: [Options] -> Int -> [Options]
-- llama a selectTheOnlyOption para cada fila. n es el numero que va a checkear si se puede dejar fijo
-- ej: [[],[],[[1,2,3],[1,2],[1,2]]] y el n = 3 devuelve [[],[],[[3],[1,2],[1,2]]]
selectSingleOptions [] _ = []
selectSingleOptions (x : xs) n = if(countPossibilities x n == 1) 
                                    then selectTheOnlyOption x n : selectSingleOptions xs n 
                                    else x : selectSingleOptions xs n

selectAllSingleOptions :: [Options] -> Int -> [Options]
-- Llama a selectSingleOptions para todos los n de 1 a 9
-- ej : [[[3,9],[3,4,5],[3,4,5,6]],[[1,5,7],[1,4,7],[1,4,7]],[[1,2,3],[1,2,4,5],[1,2,5]]] devuelve:
-- [[[9],[3,4,5],[6]],[[5],[1,4,7],[1,4,7]],[[3],[4],[5]]]
selectAllSingleOptions xs n = if(n > 9) 
                                then xs
                                else selectAllSingleOptions (selectSingleOptions xs n) (n + 1)

selectAllSingleOptionsRows ::[Options] -> [Options]
selectAllSingleOptionsRows xs = selectAllSingleOptions xs 1

selectAllSingleOptionsCol :: [Options] -> [Options]
selectAllSingleOptionsCol xs = transpose (selectAllSingleOptions (transpose xs) 1)

selectAllSingleOptionsSq :: [Options] -> [Options]
selectAllSingleOptionsSq xs = createAllSquaresOpt (selectAllSingleOptions (createAllSquaresOpt xs) 1)

cleanSingleOptions :: [Options] -> [Options]
cleanSingleOptions xs = selectAllSingleOptionsRows ( selectAllSingleOptionsCol ( selectAllSingleOptionsSq xs ) )

solve :: [Options] -> [Options]
solve xs = if getTotalOptions xs > 81 
            then solve (cleanSingleOptions (cleanOptions xs) )
            else xs 

-- creo el remove para eliminar los element que querramos de las listas
-- en este caso va a ser el 0, lo dejo generico por si lo podemos usar para otra cosa.
-- filtra los element distintos de e (recorrida de la lista)
remove element list = filter (\e -> e/=element) list

-- concateno el sudoku en una lista unica, uso el remove para sacar los 0
-- y despues calculo el largo de la lista
juntarSudoku :: Sudoku -> Int
juntarSudoku a = length ( remove 0 (concat a))



--Mensaje para ver si tiene solucion unica o no.
mensajeSolucion :: Int -> String
mensajeSolucion cantidad
 | cantidad <= 5 = "Tiene mas de una solucion... libera mas numeros man!!!"
 | cantidad <= 16 = "Tiene mas de una solucion, no podemos resolverlo =("
 | cantidad <= 80 = "Tiene solucion unica!! Ahora lo resolvemos "
 | otherwise = "Oculta alguna... ya esta resuelto"
 
 
--------------MAIN--------------
readFileWith :: String -> IO ()
readFileWith name = do
    putStrLn name
    fileHandler <- openFile name ReadMode
    contents <- hGetContents fileHandler
    printSudoku $ map (map head) (solve $ generateOptions (read contents::Sudoku))
    hClose fileHandler

showErrorOpeningFile :: IOException -> IO ()
showErrorOpeningFile exception = do
    if isDoesNotExistErrorType (ioeGetErrorType exception)
    then putStrLn ("El archivo específicado no existe :(")
    else putStrLn ("Ocurrió un error desconcido al intentar abrir el archiv :( --> " ++ show exception)

main = do
    putStrLn "Bienvenido al resolvedor de sudokus!"
    putStrLn "Ingrese el nombre del archivo que contiene el estado inicial: "
    name <- getLine
    readFileWith name `catch` showErrorOpeningFile