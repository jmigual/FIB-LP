import System.IO

---------------------- DATA STRUCTURES ---------------------

------ INTERPRET --------

class SymTable m where
    update :: m a -> String -> a -> m a
    value :: m a -> String -> Maybe a
    start :: m a 
    exists :: m a -> String -> Bool

data Memory a = Mem [(String, a)]
    deriving (Show)

instance SymTable Memory where
    update (Mem []) name val  = Mem $ (name, val):[]
    update (Mem ((s, n):xs)) name val
        | s == name     = Mem $ (s, val):xs
        | otherwise     = Mem $ (s, n):xss
        where 
            Mem xss = update (Mem xs) name val

    value (Mem []) _ = Nothing
    value (Mem ((s,n):xs)) name
        | s == name     = Just n
        | otherwise     = value (Mem xs) name

    start = Mem []

    exists (Mem []) n      = False
    exists (Mem ((x,_):xs)) n
        | x == n        = True
        | otherwise     = exists (Mem xs) n

data MemoryT a = Node String a (MemoryT a) (MemoryT a) | EmptyT
    deriving (Show)

instance SymTable MemoryT where
    update EmptyT name val  = Node name val EmptyT EmptyT
    update (Node n v l r) name val
        | n == name     = Node n val l r
        | n > name      = Node n v l (update r name val)
        | otherwise     = Node n v (update l name val) r

    value EmptyT _      = Nothing
    value (Node n v l r) name
        | n == name     = Just v
        | n > name      = value r name
        | otherwise     = value l name

    start = EmptyT

    exists EmptyT _    = False
    exists (Node n _ l r) name
        | n == name     = True
        | otherwise     = (exists l name) || (exists r name)

--------- READ COMMAND ---------

data BoolExpr a = 
    AND (BoolExpr a) (BoolExpr a)   | 
    OR (BoolExpr a) (BoolExpr a)    | 
    NOT (BoolExpr a)                |
    Gt (NumExpr a) (NumExpr a)      |
    Eq (NumExpr a) (NumExpr a)
    deriving(Read) 

data NumExpr a = 
    Var String                      | 
    Const a                         |
    Plus (NumExpr a) (NumExpr a)    |
    Minus (NumExpr a) (NumExpr a)   |
    Times (NumExpr a) (NumExpr a)   |
    Div (NumExpr a) (NumExpr a) 
    deriving (Read)

data Command a = 
    Assign (NumExpr a) (NumExpr a)              | 
    Input (NumExpr a)                           | 
    Print (NumExpr a)                           | 
    Seq [Command a]                             | 
    Cond (BoolExpr a) (Command a) (Command a)   | 
    Loop (BoolExpr a) (Command a)
    deriving(Read)

-- Instance of Show for BoolExpr
instance Show a => Show (BoolExpr a) where
    show (NOT a)    = "NOT " ++ (show a)
    show (AND a b)  = (show a) ++ " AND " ++ (show b)
    show (OR a b)   = (show a) ++ " OR " ++ (show b)
    show (Gt a b)   = (show a) ++ " > " ++ (show b)
    show (Eq a b)   = (show a) ++ " = " ++ (show b)

-- Instance of Show for NumExpr
instance Show a => Show (NumExpr a) where
    show (Var s)        = ('"':s) ++ "\""
    show (Const a)      = show a
    show (Plus a b)     = (show a) ++ " + " ++ (show b)
    show (Minus a b)    = (show a) ++ " - " ++ (show b)
    show (Times a b)    = (show a) ++ " * " ++ (show b)
    show (Div a b)      = (show a) ++ " / " ++ (show b) 
    
-- Instance of Show for Command
instance Show a => Show (Command a) where
    show (Input x)      = "INPUT " ++ (show x) ++ ";"
    show (Assign x y)   = (show x) ++ " := " ++ (show y) ++ ";"
    show (Print x)      = "PRINT " ++ (show x) ++ ";"
    show c              = showIdent 0 0 c
        where 
        showIdent :: Show a => Int -> Int -> Command a -> String
        showIdent _ _ (Seq [])  = []
        showIdent n i (Seq (x:[]))
            | i == 0        = showIdent n i x
            | otherwise     = ' ':(showIdent n (i-1) (Seq (x:[])))
        showIdent n i (Seq (x:xs)) 
            | i == 0        = (showIdent n i x) ++ '\n':(showIdent n n (Seq xs))
            | otherwise     = ' ':(showIdent n (i-1) (Seq (x:xs)))
        showIdent n i (Cond b ii ee) = "IF " ++ (show b) ++ " THEN\n" ++ showIf ++ elseS ++ showEl ++ endS
            where 
                showIf      = showIdent (n+2) (n+2) ii
                elseS       = '\n':(addSpaces n "ELSE\n")
                showEl      = showIdent (n+2) (n+2) ee
                endS        = '\n':(addSpaces n "END")
        showIdent n i (Loop b l) = "WHILE " ++ (show b) ++ doS ++ loop ++ endS
            where
                doS         = '\n':(addSpaces n "DO\n")
                loop        = showIdent (n+2) (n+2) l
                endS        = '\n':(addSpaces n "END")
        showIdent n i c
            | i == 0        = show c
            | otherwise     = ' ':(showIdent n (i-1) c)

        -- Adds n spaces
        addSpaces :: Int -> String -> String
        addSpaces n s
            | n == 0        = s
            | otherwise     = ' ':(addSpaces (n-1) s)

-------------------------- AUXILIAR FUNCTIONS -----------------------

-- Returns true if it's Just
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

-- Returns true if is a right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False

-- Returns true if is left
isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left  _) = True

-- Returns the left
fromLeft :: Either a b -> a
fromLeft (Right _) = error "fromLeft: It's not left"
fromLeft (Left  m) = m

-- Returns the right
fromRight :: Either a b -> b
fromRight (Right m) = m
fromRight (Left  _) = error "fromRight: It's not right"

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn s1 s2 = fst $ splitOnAux s1 s2


splitOnAux :: Eq a => [a] -> [a] -> ([[a]], Bool)
splitOnAux _ [] = ([[]], False)
splitOnAux xs (y:ys)
    | b2        = (splitOn xs r, True)
    | b         = ([y]:res, False)
    | otherwise = case res of
                    [[]]    -> ([[y]], False)
                    (m:[])  -> ([y:m], False)
                    (m:ms)  -> (((y:m):ms), False)
    where
        (r, b2) = subStr xs (y:ys)
        (res,b) = splitOnAux xs ys

        subStr :: Eq a => [a] -> [a] -> ([a], Bool)
        subStr []     ys        = (ys, True)
        subStr _      []        = ([], False)
        subStr (x:xs) (y:ys)
            | x == y    = (r, True && b)
            | otherwise = (y:ys, False)
            where
                (r, b)   = subStr xs ys

-- Separates the next word from the passed String
dropNextWord ::  String -> (String, String)
dropNextWord s  = (w, d)
    where 
        begin       = dropWhile (\x -> x ==' ' || x == '\n') s
        (w, r)      = span (\x -> x /=' ' && x  /= '\n') begin
        d           = dropWhile (==' ') r

-- Removes the ';' and the remaining spaces or endlines
dropNextLine :: String -> (String, String)
dropNextLine s = (b, e)
    where
        (b, dirty)  = span (\x -> x /= ';' && x /= '\n') s
        e           = dropWhile (\x -> x == ';' || x == ' ' || x == '\n') dirty

-- Reads a String and returns a NumExpr (if it founds a String name
-- creates a Var "name", otherwise creates a const "Number")
readStringNum :: Read a => String -> NumExpr a
readStringNum s
    | isNumber s    = Const $ read s
    | otherwise     = Var s

-- Concatenates constructors
concatCons :: Read a => (a -> a -> a) -> [a] -> a
concatCons _ []     = error "concatCons: Empty List"
concatCons _ (x:[]) = x
concatCons f (x:xs) = f x (concatCons f xs)

-- Reads a recusive expression, pass the constructor, the recursive function and the split string
readRecExpr :: Read a => (String -> a) -> (a -> a -> a) -> String -> String -> a
readRecExpr f c split s = concatCons c mapped
    where
        subs    = splitOn split s
        mapped  = map f subs

-- Gets the integer division between two numbers
myDiv :: (Num a, Ord a) => a -> a -> Either a String
myDiv a b
    | b == 0                        = Right "div: Error, division by 0"
    | (x >= y) && (isLeft rDivM)    = Left $ getSign a b (1 + rDiv)
    | otherwise = Left 0
    where 
        x           = abs a
        y           = abs b
        rDivM       = myDiv (x - y) y
        Left rDiv   = rDivM
        getSign :: (Num a, Ord a) => a -> a -> a -> a
        getSign n m res
            | n > 0 && m > 0    = res
            | n > 0 && m < 0    = (-res)
            | n < 0 && m > 0    = (-res)
            | otherwise         = res

isNumber :: String -> Bool
isNumber s = foldl (&&) True (map (\x -> '0' <= x && x <= '9' || x == '.') s)

------------------------- MAIN CODE --------------------------

-- Evaluates a numExpr to a number

evalNumExpr :: (SymTable m, Num a, Ord a) => m a -> NumExpr a -> Either a String
evalNumExpr mem (Var x)
    | isJust var    = Left val
    | otherwise     = Right $ "Var '" ++ x ++ "' has no defined value"
    where
        var         = value mem x
        Just val    = var
evalNumExpr mem (Const n)       = Left n
evalNumExpr mem (Minus n m)     = evalNumExprAux mem (-) n m
evalNumExpr mem (Plus n m)      = evalNumExprAux mem (+) n m
evalNumExpr mem  (Times n m)    = evalNumExprAux mem (*) n m
evalNumExpr mem  (Div n m)
    | isRight xM    = xM
    | isRight yM    = yM
    | otherwise     = myDiv (fromLeft xM) (fromLeft yM)
    where
        xM      = evalNumExpr mem n
        yM      = evalNumExpr mem m


evalNumExprAux :: (SymTable m, Num a, Ord a) => m a -> (a -> a -> a) -> NumExpr a -> NumExpr a -> Either a String
evalNumExprAux mem f n m
    | isRight xM    = xM
    | isRight yM    = yM
    | otherwise     = Left (f x y)
    where
        xM      = evalNumExpr mem n
        yM      = evalNumExpr mem m
        Left x  = xM
        Left y  = yM

-- Evaluates a boolean expression
evalBoolExpr :: (SymTable m, Num a, Ord a) => m a -> BoolExpr a -> Either Bool String
evalBoolExpr mem (Gt  n m)  = evalBoolExprAux  mem (>)  n m
evalBoolExpr mem (Eq  n m)  = evalBoolExprAux  mem (==) n m
evalBoolExpr mem (AND n m)  = evalBoolExprAuxB mem (&&) n m
evalBoolExpr mem (OR  n m)  = evalBoolExprAuxB mem (||) n m
evalBoolExpr mem (NOT n)
    | isRight bM    = Right (fromRight bM)
    | otherwise     = Left (not $ fromLeft bM)
    where
        bM          = evalBoolExpr mem n

-- Evaluates two boolean expressions and applies the desired boolean function
evalBoolExprAuxB :: (SymTable m, Num a, Ord a) => m a -> (Bool -> Bool -> Bool) -> BoolExpr a -> BoolExpr a -> Either Bool String
evalBoolExprAuxB mem f bE1 bE2
    | isRight b1M   = Right (fromRight b1M)
    | isRight b2M   = Right (fromRight b2M)
    | otherwise     = Left (f (fromLeft b1M) (fromLeft b2M))
    where
        b1M         = evalBoolExpr mem bE1
        b2M         = evalBoolExpr mem bE2

-- Evaluates two num expressions and applies the desired function to the numbers to return a boolean
evalBoolExprAux :: (SymTable m, Num a, Ord a) => m a -> (a -> a -> Bool) -> NumExpr a -> NumExpr a -> Either Bool String
evalBoolExprAux mem f num1 num2
    | isRight n1M   = Right (fromRight n1M)
    | isRight n2M   = Right (fromRight n2M)
    | otherwise     = Left (f (fromLeft n1M) (fromLeft n2M))
    where
        n1M         = evalNumExpr mem num1
        n2M         = evalNumExpr mem num2

-- Reads a numeric expression and returns the remaining string
readNumExpr :: Read a => String -> (NumExpr a, String)
readNumExpr s = (plus, r)
    where 
        (str, r)    = dropNextLine s
        times       = \x -> readRecExpr readStringNum Times " * " x
        divs        = \y -> readRecExpr times Div " / " y
        minus       = \z -> readRecExpr divs Minus " - " z
        plus        = readRecExpr minus Plus " + " str

-- Takes items until a command from the list is found
takeCommand :: String -> [String] -> (String, String) 
takeCommand [] _     = ("", "")
takeCommand s xs
    | elem com xs    = ("", s)
    | expr == ""        = (com, cuar)
    | otherwise         = (com ++ " " ++ expr, cuar)
    where 
        (com, cua)      = dropNextWord s
        (expr, cuar)    = takeCommand cua xs

-- Read bool comparison, 4th level of recursion (last)
readBoolCom :: Read a => String -> BoolExpr a
readBoolCom s
    | com == ">" = Gt nExpr1 nExpr2
    | com == "=" = Eq nExpr1 nExpr2
    | otherwise     = error "readBoolCom: Unknown comparator"
    where 
        (expr1, dirty)  = takeCommand s [">", "="]
        (com, expr2)    = dropNextWord dirty
        nExpr1          = fst $ readNumExpr expr1
        nExpr2          = fst $ readNumExpr expr2

-- Read bool NOT, 3rd level of recursion
readBoolNot :: Read a => String -> BoolExpr a
readBoolNot s
    | com == "NOT"  = NOT (readBoolCom remaining)
    | otherwise     = readBoolCom s
    where 
        (com, remaining)    = dropNextWord s

-- Reads a boolean expression and returns the remaining string
readBool :: Read a => String -> (BoolExpr a, String)
readBool s = (ors, dirty)
    where
        (expr, dirty)   = takeCommand s ["THEN", "DO"]
        ands            = \x -> readRecExpr readBoolNot AND " AND " x
        ors             = readRecExpr ands OR " OR " expr

-- Creates a PRINT command creating also a NumExpr
readCommandPrint :: Read a => String -> (Command a, String)
readCommandPrint s = (Print num, remain)
    where
        noP             = snd $ dropNextWord s       -- Remove PRINT
        (num, remain)   = readNumExpr noP            -- Read NumExpr

-- Creates a command that uses a variable name
readCommandInput :: Read a => String -> (Command a, String)
readCommandInput s  = (Input (Var var), rema)
    where
        (var, rema) = dropNextLine $ snd $ dropNextWord s

-- Creates a If Command from a String
readCommandIf :: Read a => String -> (Command a, String)
readCommandIf s = (Cond b ifCom elCom, remaining)
    where 
        noIf            = snd $ dropNextWord s      -- Remove IF 
        (b, bRem)       = readBool noIf             -- Read boolean expression
        thenRem         = snd $ dropNextLine bRem   -- Remove THEN
        (ifCom, ifRem)  = readCommandSeq thenRem    -- Read if commands, stops when founds the ELSE
        eRem            = snd $ dropNextWord ifRem  -- Remove ELSE
        (elCom, elRem)  = readCommandSeq eRem       -- Read else commands, stops when founds END
        remaining       = snd $ dropNextWord elRem  -- Remove END

-- Creates a While Command from a String
readCommandWhile :: Read a => String -> (Command a , String)
readCommandWhile s      = (Loop b wCom, remaining)
    where
        noWhile         = snd $ dropNextWord s      -- Remove WHILE
        (b, bRem)       = readBool noWhile          -- Read boolean expression
        dRem            = snd $ dropNextWord bRem   -- Remove DO
        (wCom, wRem)    = readCommandSeq dRem       -- Read sequence
        remaining       = snd $ dropNextWord wRem   -- Remove END

-- Creates an Assign Command from a String
readCommandAssign :: Read a => String -> (Command a, String)
readCommandAssign s     = (Assign (Var var) expr, remain)
    where
        (var, dirty)    = dropNextWord s            -- Get var
        dExpr           = snd $ dropNextWord dirty  -- Remove ':='
        (expr, remain) = readNumExpr dExpr          -- Get expression and next line

-- Creates a Seq Command from a String
readCommandSeq :: Read a => String -> (Command a, String)
readCommandSeq []       = (Seq [], [])
readCommandSeq s
    | com == "INPUT"    = readCommandSomSeq readCommandInput s
    | com == "IF"       = readCommandSomSeq readCommandIf s
    | com == "WHILE"    = readCommandSomSeq readCommandWhile s
    | com == "ELSE"     = (Seq [], s)
    | com == "END"      = (Seq [], s)
    | com == "PRINT"    = readCommandSomSeq readCommandPrint s
    | otherwise         = readCommandSomSeq readCommandAssign s
    where 
        com             = fst $ dropNextWord s
        -- Creates a Seq Command from a String and the selected function
        readCommandSomSeq :: Read a => (String -> (Command a, String)) -> String -> (Command a, String)
        readCommandSomSeq _ [] = (Seq [], [])
        readCommandSomSeq f x  = (Seq (singleCommand:xs), remaining)
            where   
                (singleCommand, remAux) = f x                   -- Apply the desired function
                (Seq xs, remaining)     = readCommandSeq remAux -- Call the main function to read another command

-- Creates an AST from the input String
readCommand :: Read a => String -> Command a
readCommand s = fst $ readCommandSeq s

-- Interpret Command Sequence
interpretCommand :: (SymTable m, Num a, Ord a) => m a -> [a] -> Command a -> ((Either [a] String), m a, [a])
interpretCommand mem inI    (Seq [])       = (Left [], mem, inI)
interpretCommand mem inI    (Seq (x:xs)) 
    | isRight outL  = (outL, memR, inp)
    | isRight outF  = (outF, memF, inpF)
    | otherwise     = (Left (out ++ o), memF, inpF)
    where 
        (outL, memR, inp)   = interpretCommand mem inI x
        Left out             = outL
        (outF, memF, inpF)  = interpretCommand memR inp (Seq xs)
        Left o              = outF

-- Interpret Command Input
interpretCommand mem []     (Input _)       = (Right "Empty input", mem, [])
interpretCommand mem (x:xs) (Input (Var n)) = (Left [], update mem n x, xs)
interpretCommand mem xs     (Input _)       = (Right "Wrong Input command", mem, xs)

-- Interpret Command Print
interpretCommand mem i      (Print numE)
    | isRight varM  = (Right err, mem, i)
    | otherwise     = (Left [val], mem, i)
    where 
        varM        = evalNumExpr mem numE
        Left val    = varM
        Right err   = varM

-- Interpret Command Assign
interpretCommand mem i      (Assign (Var n) numE)
    | isRight varM      = (Right err, mem, i)
    | otherwise         = (Left [], update mem n val, i)
    where
        varM        = evalNumExpr mem numE
        Left val    = varM
        Right err   = varM

-- Interpret Command If
interpretCommand mem xs     (Cond bol ifC elC)
    | isRight bM    = (Right (fromRight bM), mem, xs)
    | fromLeft bM   = interpretCommand mem xs ifC
    | otherwise     = interpretCommand mem xs elC
    where
        bM      = evalBoolExpr mem bol

-- Interpret Command Loop
interpretCommand mem inp     (Loop bol com)
    | isRight bM    = (Right (fromRight bM), mem, inp)
    | fromLeft bM   = if isRight r 
                        then (r, m, i)
                        else if isRight r2
                            then (r2, m2, i2)
                            else (Left $ o ++ o2, m2, i2)
    | otherwise     = (Left [], mem, inp)
    where
        bM              = evalBoolExpr mem bol
        (r, m, i)       = interpretCommand mem inp com
        (r2, m2, i2)    = interpretCommand m i (Loop bol com)
        Left o          = r
        Left o2         = r2

-- Interpretes a program assigning the desired SymTable
interpretProgram :: (Num a, Ord a) => [a] -> Command a -> Either [a] String
interpretProgram i c = ret
    where
        -- res     = interpretCommand EmptyT i c
        res         = interpretCommand (Mem []) i c
        (ret, _, _) = res


expand :: Command a -> Command a
expand (Seq [])     = Seq []
expand (Seq (x:xs)) = Seq $ (expand x):sE
    where
        Seq sE = expand $ Seq xs
expand (Loop b xs)  = Loop b (expand xs)
expand (Cond (AND b1 b2) i e) = expand (Cond b1 (Seq [Cond b2 i e]) e)
expand (Cond (OR  b1 b2) i e) = expand (Cond b1 i (Seq [Cond b2 i e]))
expand (Cond boolExpr    i e) = Cond boolExpr (expand i) (expand e)
expand com                    = com

simplify :: Command a -> Command a
simplify (Seq [])                = Seq []
simplify (Seq ((Input _):xs))    = simplify (Seq xs)
simplify (Seq ((Print _):xs))    = simplify (Seq xs)
simplify (Seq ((Assign _ _):xs)) = simplify (Seq xs)
simplify (Seq (x:xs))            = Seq $ (simplify x):ms
    where
        Seq ms = (simplify (Seq xs))
simplify (Cond b i e)            = Cond b (simplify i) (simplify e)
simplify (Loop b c)              = Loop b (simplify c)
simplify (Input _)               = Seq []
simplify (Print _)               = Seq []
simplify (Assign _ _)            = Seq []


-- Reads input and parses it to the type a until the * symbol is found
readInput :: Read a => IO [a]
readInput =
    do
        l <- getLine
        if l /= "*" 
            then do
                let n = read l in do 
                    arr <- readInput
                    return $ n:arr
            else 
                return []

main :: IO ()
main = 
    do
        h <- openFile "codiProva.txt" ReadMode
        s <- hGetContents h
        let c = (readCommand s :: Command Integer) in do
            i <- readInput :: IO [Integer]
            print c
            let res = interpretProgram i c in print res
            putStrLn "\nExpanded: "
            print $ expand c
            putStrLn "\nSimplified: "
            print $ simplify c
            putStrLn "\nBoth: "
            print $ simplify (expand c)
            return ()