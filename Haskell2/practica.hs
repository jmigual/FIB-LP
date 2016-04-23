data Point = P Int Int deriving(Show, Read, Eq)
data Line = L Point Point deriving(Show, Read, Eq)
data Board = B Int [Line] deriving(Read)

instance Show Board where
    show (B n1 b1) = showH n1 0 ++ showB (B n1 b1) 0 0 0
        where
            showH n i
                | i < n     = "  " ++ show i ++ showH n (i+1)
                | otherwise = "\n"
            showB (B n b) i j l
                | l == (2*n)-1  = ""
                | j == n        = "\n"  ++ showB (B n b) i2 0 (l+1)
                | j == 0 && even l = show i ++ " ·" ++ char0 ++ continue
                | even l        = "·"   ++ char0 ++ continue
                | odd  l        = "  "  ++ char1 ++ continue
                | otherwise     = "  "  ++ continue
                where 
                    l0 = L (P i j) (P i (j+1))
                    l1 = L (P i j) (P (i+1) j)
                    i2 = if even l then i else i+1
                    char0 = if existsL b l0 then "--" else "  "
                    char1 = if existsL b l1 then "|" else " "
                    continue = showB (B n b) i (j+1) l

isValidLine:: Int -> Line -> Bool
isValidLine n (L (P x1 y1) (P x2 y2)) = len && bet
    where
        len = abs (x1 - x2) + abs (y1 - y2) == 1
        bet = 0 <= x1 && x1 < n && 0 <= x2 && x2 < n && 0 <= y1 && y1 < n && 0 <= y2 && y2 < n

isValidMove:: Board -> Line -> Bool
isValidMove (B n xs) l1 = existsL xs l1 && isValidLine n l1

existsL:: [Line] -> Line -> Bool
existsL [] _    = False
existsL (lx:xs) ly
    | lx == ly  = True
    | otherwise = existsL xs ly
 
readLine:: IO Line
readLine = do
    putStr "Enter first  coordinates (row column):"
    line1 <- getLine
    map (\x->read x::Int) (words line1)
    putStr "Enter second coordinates (row column):"
    line2 <- getLine
    map (\x->read x::Int) (words line2)


gameLoop:: Board -> Int -> Int -> IO ()
gameLoop b n m =

    gameLoop b n m

main:: IO ()
main = do
    putStr "Introdueix el tamany del tauler: "
    return ()