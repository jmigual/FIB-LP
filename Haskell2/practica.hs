data Point = P Int Int deriving(Show, Read, Eq)
data Line = L Point Point deriving(Show, Read)
data Board = B Int [Line] deriving(Read)

instance Eq Line where
  (==) (L p1 q1) (L p2 q2) = (p1 == p2 && q1 == q2) || (p1 == q2 && q1 == p2)

instance Show Board where
    show (B n1 b1) = showH n1 0 ++ showB (B n1 b1) 0 0 0
        where
            showH n i
                | i < n     = "  " ++ show i ++ showH n (i+1)
                | otherwise = "\n"
            showB (B n b) i j l
                | l == (2*n)-1  = ""
                | j == n        = "\n"  ++ showB (B n b) i2 0 (l+1)
                | j == 0 && even l = show i ++ " •" ++ char0 ++ continue
                | even l        = "•"   ++ char0 ++ continue
                | odd  l        = "  "  ++ char1 ++ continue
                | otherwise     = "  "  ++ continue
                where 
                    l0 = L (P i j) (P i (j+1))
                    l1 = L (P i j) (P (i+1) j)
                    i2 = if even l then i else i+1
                    char0 = if existsL b l0 then "——" else "  "
                    char1 = if existsL b l1 then "|" else " "
                    continue = showB (B n b) i (j+1) l

isHorizontal :: Line -> Bool
isHorizontal (L (P x1 _) (P x2 _)) = x1 == x2

isVertical :: Line -> Bool
isVertical  (L (P _ y1) (P _ y2)) = y1 == y2
                    
isValidLine:: Int -> Line -> Bool
isValidLine n (L (P x1 y1) (P x2 y2)) = len && bet
    where
        len = abs (x1 - x2) + abs (y1 - y2) == 1
        bet = 0 <= x1 && x1 < n && 0 <= x2 && x2 < n && 0 <= y1 && y1 < n && 0 <= y2 && y2 < n

isValidMove:: Board -> Line -> Bool
isValidMove (B n xs) l1 = (not $ existsL xs l1) && isValidLine n l1

existsL:: [Line] -> Line -> Bool
existsL [] _    = False
existsL (lx:xs) ly
    | lx == ly  = True
    | otherwise = existsL xs ly

boxH :: Board -> Line -> Int -> Bool
boxH b (L (P x1 y1) (P x2 y2)) x0 = 
  e xs (L (P x0 y1) (P x0 y2)) && e xs (L (P x1 y1) (P x0 y1)) && e xs (L (P x1 y2) (P x0 y2))
  where 
    e = existsL
    (B _ xs) = b
        
boxV :: Board -> Line -> Int -> Bool
boxV b (L (P x1 y1) (P x2 y2)) y0 =
  e xs (L (P x1 y0) (P x2 y0)) && e xs (L (P x1 y1) (P x1 y0)) && e xs (L (P x2 y1) (P x2 y0))
  where 
    e = existsL
    (B _ xs) = b
        
getBoxesH :: Board -> Line -> Int
getBoxesH b l
  | b1 && b2    = 2
  | b1 || b2    = 1
  | otherwise   = 0
  where
    b1 = boxH b l (x1-1)
    b2 = boxH b l (x1+1)
    (L (P x1 y1) (P x2 y2)) = l
    
getBoxesV :: Board -> Line -> Int
getBoxesV b l
  | b1 && b2    = 2
  | b1 || b2    = 1
  | otherwise   = 0
  where
    b1 = boxV b l (y1-1)
    b2 = boxV b l (y1+1)
    (L (P x1 y1) (P x2 y2)) = l
  
    
getBoxes :: Board -> Line -> Int
getBoxes b l
  | isHorizontal l  = getBoxesH b l
  | otherwise       = getBoxesV b l
  
 
addLine :: Board -> Line -> Int -> IO (Board, Int)
addLine (B n xs) l s = return $ (B n (l:xs), s + (getBoxes (B n xs) l))
 
readLine:: Board -> IO Line
readLine b = do
    putStrLn "Enter first  coordinates (row column): "
    line1 <- getLine
    let (x1:y1:_) = map (\x->read x::Int) (words line1) in do
      
      putStrLn "Enter second coordinates (row column): "
      line2 <- getLine
      let (x2:y2:_) = map (\x->read x::Int) (words line2) in do
        
        let l = (L (P x1 y1) (P x2 y2)) in do
          if isValidMove b l
             then do return l
             else do
              putStrLn "Not a valid line!"
              readLine b

doMovement :: Board -> Int -> (Board -> IO Line) -> IO (Board, Int)
doMovement b s f = do
  line <- f b
  addLine b line s
  
isGameFinished :: Board -> Bool
isGameFinished (B n xs) = (length xs) == (2*n0*(n0+1)) where n0 = n-1

finishGame :: Int -> Int -> IO ()
finishGame 
  | n > m       = do { putStrLn "Player 1" ; return () }
  | otherwise   = do { putStrLn "Player 2" ; return() }

gameLoop:: Board -> Int -> Int -> IO ()
gameLoop b n m = do
  putStrLn "Score"
  putStrLn ("Player 1: " ++ (show n))
  putStrLn ("Player 2: " ++ (show m))
  putStrLn "Current board:"
  print b
  
  putStrLn "Player 1 Move"
  (b1, n1) <- doMovement b n readLine
  print b1
  
  if isGameFinished b1
     then do 
       putStr "Game Finished! winner: " 
       
     else do
  
  putStrLn "Player 2 Move"
  (b2, m1) <- doMovement b1 m readLine
  
  if isGameFinished b2
    then do 
      putStr "Game Finished! winner: " 
      if n > m then do putStrLn "Player 1"
               else do putStrLn "Player 2"
      return ()
    else do
      
  putStrLn ""
  putStrLn "--------------------"
  gameLoop b2 n1 m1

main:: IO ()
main = do
    putStrLn "Introdueix el tamany del tauler: "
    line <- getLine 
    gameLoop (B ((read line :: Int) + 1) []) 0 0
    return ()