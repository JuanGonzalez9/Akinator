import Data.List

type Sudoku = [[Int]]

printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . unlines . map (unwords . map show)

s1 :: Sudoku
s1 = [[0,0,0,2,6,0,7,0,1],[6,8,0,0,7,0,0,9,0],[1,9,0,0,0,4,5,0,0],
       [8,2,0,1,0,0,0,4,0],[0,0,4,6,0,2,9,0,0],[0,5,0,0,0,3,0,2,8],
       [0,0,9,3,0,0,0,7,4],[0,4,0,0,5,0,0,3,6],[7,0,3,0,1,8,0,0,0]]

s2 :: Sudoku
s2 = [[5,3,4,6,7,8,9,1,2],[6,7,2,1,9,5,3,4,8],[1,9,8,3,4,2,5,6,7],
       [8,5,9,7,6,1,4,2,3],[4,2,6,8,5,3,7,9,1],[7,1,3,9,2,4,8,5,6],
       [9,6,1,5,3,7,2,8,4],[2,8,7,4,1,9,6,3,5],[3,4,5,2,8,6,1,7,9]]

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

createSquares :: Int -> Sudoku -> [[Int]]
-- crea una lista de listas, con los 3 primeros cuadrados, o los siguientes 3, o los ultimos 3
-- row puede ser 0, 1 o 2
createSquares row xs = [createSquare 3 0 x,createSquare 3 1 x,createSquare 3 2 x]
    where x = drop (3*row) xs

createAllSquares :: Sudoku -> Sudoku
-- crea una copia del sudoku, donde cada fila es uno de los cuadrados
createAllSquares xs = createSquares 0 xs ++ createSquares 1 xs ++ createSquares 2 xs 

squaresAreValid :: Sudoku -> Bool
squaresAreValid xs = rowsAreValid $ createAllSquares xs

isValid :: Sudoku -> Bool
isValid s = rowsAreValid s && colsAreValid s && squaresAreValid s
