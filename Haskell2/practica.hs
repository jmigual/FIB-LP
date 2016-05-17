data Point = P Int Int deriving(Show, Read, Eq)
data Line = L Point Point deriving(Show, Read)
data Board = B Int [Line] deriving(Read)
data Orientation = Horizontal | Vertical | Both deriving(Show, Read)

instance Eq Line where
  (==) (L p1 q1) (L p2 q2) = (p1 == p2 && q1 == q2) || (p1 == q2 && q1 == p2)

instance Eq Orientation where
  (==) Horizontal Horizontal  = True
  (==) Horizontal Vertical    = False
  (==) Vertical   Horizontal  = False
  (==) Vertical   Vertical    = True
  (==) Both       _           = True
  (==) _          Both        = True

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

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

fromJust :: Maybe a -> a
fromJust (Just x) = x

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

toDown :: String -> String
toDown [] = []
toDown (x:xs) 
  | x < 'Z' && x > 'A' = (toEnum((fromEnum x) - (fromEnum 'A') + (fromEnum 'a'))):toDown xs
  | otherwise = x:toDown xs
  
                    
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
boxH (B _ xs) (L (P x1 y1) (P x2 y2)) x0 = 
  e xs (L (P x0 y1) (P x0 y2)) && e xs (L (P x1 y1) (P x0 y1)) && e xs (L (P x1 y2) (P x0 y2))
  where e = existsL
        
boxV :: Board -> Line -> Int -> Bool
boxV (B _ xs) (L (P x1 y1) (P x2 y2)) y0 =
  e xs (L (P x1 y0) (P x2 y0)) && e xs (L (P x1 y1) (P x1 y0)) && e xs (L (P x2 y1) (P x2 y0))
  where e = existsL
    
        
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
 
readLine:: Board -> IO Line
readLine b = do
  putStrLn "Enter first  coordinates (row column) and direction [h|v]: "
  line1 <- getLine
  -- Read two ints and a string containing either "h" or "v"
  let (xS:yS:z:_) = (words line1); x = read xS :: Int; y = read yS :: Int in do
    if (toDown z) == "h" then checkLine b (L (P x y) (P x (y+1)))
    else checkLine b (L (P x y) (P (x+1) y))
  where
    checkLine :: Board -> Line -> IO Line
    checkLine b l
      | isValidMove b l   = do return l
      | otherwise         = do
        putStrLn "Not a valid line!"
        readLine b

findEmptyLine :: Board -> Int -> Int -> Orientation -> Maybe Line
findEmptyLine (B n xs) i j ori
  | i < (n-1) && j < (n-1) && (not $ existsL xs l1) && (ori == Vertical)    = Just l1
  | i < (n-1) && j < (n-1) && (not $ existsL xs l2) && (ori == Horizontal)  = Just l2
  | i < (n-1) && j < n     && (not $ existsL xs l1) && (ori == Vertical)    = Just l1
  | i < n     && j < (n-1) && (not $ existsL xs l2) && (ori == Horizontal)  = Just l2
  | j >= n    = findEmptyLine b1 (i+1) 0 ori
  | i < n     = findEmptyLine b1 i (j+1) ori
  | otherwise = Nothing
  where
    l1 = L (P i j) (P (i+1) j)
    l2 = L (P i j) (P i (j+1))
    b1  = (B n xs)

findEmptyBox :: Board -> Int -> Int -> Maybe Line
findEmptyBox b i j
  | (isJust l1) && (getBoxes b line1) > 0 = Just line1
  | (isJust l2) && (getBoxes b line2) > 0 = Just line2
  | isJust l1                             = findEmptyBox b x1 (y1+1)
  | otherwise                             = Nothing
  where
    l1 = findEmptyLine b i j Vertical
    l2 = findEmptyLine b i j Horizontal
    Just line1 = l1
    Just line2 = l2
    (L (P x1 y1) (P x2 y2)) = line1

cpuRandom :: Board -> IO Line
cpuRandom b
  | isJust line   = return (fromJust line)
  | otherwise     = return (L (P 0 0) (P 0 1))
  where 
    line = findEmptyLine b 0 0 Both

              
cpuBest :: Board -> IO Line
cpuBest b 
  | isJust l  = return $ fromJust l
  | otherwise = return $ fromJust $ findEmptyLine b 0 0 Both
  where 
    l = findEmptyBox b 0 0

-- The function f decides which line to add and then the line is added to the board,
-- the line must be a valid line
doMovement :: Board -> Int -> (Board -> IO Line) -> IO (Board, Int)
doMovement b s f = do
  line <- f b
  addLine b line s
    where
      addLine :: Board -> Line -> Int -> IO (Board, Int)
      addLine (B n xs) l s = return $ (B n (l:xs), s + (getBoxes (B n xs) l))
  
isGameFinished :: Board -> Bool
isGameFinished (B n xs) = (length xs) == (2*n0*(n0+1)) where n0 = n-1

finishGame :: Board -> Int -> Int -> IO ()
finishGame b n m
  | n > m       = do { putStrLn $ st ++ "Player 1"; sc; fin; return () }
  | otherwise   = do { putStrLn $ st ++ "Player 2"; sc; fin; return () }
  where 
    sc = do { putStrLn "Final Score"; putStrLn $ "Player 1: " ++ (show n); putStrLn $ "Player 2: " ++ (show m) }
    st = "Game Finished! winner: "
    fin = do { putStrLn "Final board"; print b }

gameLoop:: (Board -> IO Line) -> (Board -> IO Line) -> Board -> Int -> Int -> IO ()
gameLoop p1 p2 b n m = do
  putStrLn "Score"
  putStrLn ("Player 1: " ++ (show n))
  putStrLn ("Player 2: " ++ (show m))
  putStrLn "Player 1, is your turn!"
  putStrLn "Current board:"
  print b
  (b1, n1) <- doMovement b n p1
  
  if isGameFinished b1
     then finishGame b1 n1 m
     else do
  
  putStrLn "Player 2, is your turn!"
  putStrLn "Current board:"
  print b1
  (b2, m1) <- doMovement b1 m p2
  
  if isGameFinished b2
    then finishGame b2 n1 m1
    else do
      
  putStrLn ""
  putStrLn "--------------------"
  gameLoop p1 p2 b2 n1 m1

main:: IO ()
main = do
  putStrLn "Choose the game mode for player1 [human|CPURandom|CPUBest]"
  line1 <- getLine
  putStrLn "Choose the game mode for player2 [human|CPURandom|CPUBest]"
  line2 <- getLine
  putStrLn "Choose the size of the board: "
  line <- getLine 
  gameLoop (getPlayer line1) (getPlayer line2) (B ((read line :: Int) + 1) []) 0 0
  return ()
  where
    getPlayer :: String -> (Board -> IO Line)
    getPlayer s0
      | s == "human"                    = readLine
      | s == "cpurandom" || s == "cpur" = cpuRandom
      | otherwise                       = cpuBest
      where s = toDown s0