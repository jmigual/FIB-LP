import System.IO
import Data.List
import Data.List.Split

---------------------- MAIN CODE ---------------------

data BoolExpr a = 
    AND (BoolExpr a) (BoolExpr a) | 
    OR (BoolExpr a) (BoolExpr a) | 
    NOT (BoolExpr a) |
    Gt (NumExpr a) (NumExpr a) |
    Eq (NumExpr a) (NumExpr a)
    deriving(Read, Show) 
{-
instance Show a => Show (BoolExpr a) where
    show (AND a b)  = (show a) ++ " AND " ++ (show b)
    show (OR a b)   = (show a) ++ " OR " ++ (show b)
    show (NOT a)    = "NOT " ++ (show a)
    show (Gt a b)   = (show a) ++ " > " ++ (show b)
    show (Eq a b)   = (show a) ++ " = " ++ (show b)
-}

data NumExpr a = 
    Var String | 
    Const a |
    Plus (NumExpr a) (NumExpr a) |
    Minus (NumExpr a) (NumExpr a) |
    Times (NumExpr a) (NumExpr a) |
    Div (NumExpr a) (NumExpr a) 
    deriving (Read, Show)
{-
instance Show a => Show (NumExpr a) where
    show (Var s)        = s
    show (Const a)      = show a
    show (Plus a b)     = (show a) ++ " + " ++ (show b)
    show (Minus a b)    = (show a) ++ " - " ++ (show b)
    show (Times a b)    = (show a) ++ " * " ++ (show b)
    show (Div a b)      = (show a) ++ " / " ++ (show b) 
    -}

data Command a = 
    Assign (NumExpr a) (NumExpr a) | 
    Input (NumExpr a) | 
    Print (NumExpr a) | 
    Seq [Command a] | 
    Cond (BoolExpr a) (Command a) (Command a) | 
    Loop (BoolExpr a) (Command a)
    deriving(Read, Show)

{-instance Show a => Show (Command a) where
     show (Assign x y)  = (show x) ++ " := " ++ (show y) ++ ";\n"
     show (Input x)     = "INPUT " ++ (show x) ++ ";\n"
     show (Print x)     = "PRINT " ++ (show x) ++ ";\n"
     show (Seq [])      = []
     show (Seq (x:xs))  = (show x) ++ ("\n") ++ (show (Seq xs))
     show (Cond b i e)  = "IF " ++ (show b) ++ " THEN\n" ++ (show i) ++ "ELSE\n" ++ (show e) ++ "END"
     show (Loop b c)    = "WHILE " ++ (show b) ++ "\nDO\n" ++ (show c) ++ "END\n"
-}
-- Creates a Input Command from a String
readInput :: String -> (Command a, String)
readInput s = Input (Var xs)
    where xs = dropWhile (== ' ') (dropWhile (/= ' ') s)

readSeq :: String -> (Command a, String)
readSeq s
    | com == "INPUT"    = 
    {-| com == "IF"       =
    | com == "WHILE"    =
    | com == "DO"       =
    | com == "PRINT"    =
    | com == "END"      = -}
    | otherwise         = (Seq [], [])
    where 
        

        (inst, queueD)  = span (/= ';') s
        com             = takeWhile(\x -> x /= ' ') inst
        queue           = drop 1 queueD
        (Seq xs)        = readSeq queue

-- Creates an AST from the input String
readCommand :: Num a => String -> Command a
readCommand s = first(readSeq s)
    

main = 
    do
        h <- openFile "codiPractica.txt" ReadMode
        s <- hGetContents h
        putStrLn (show (readCommand s))
