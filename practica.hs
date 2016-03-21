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

dropNextWord ::  String -> String
dropNextWord s  = dropWhile (== ' ') (dropWhile (/=' ') s)

-- Creates a Input Command from a String
readCommandInput :: String -> (Command a, String)
readCommandInput s = (Input (Var fst), last)
    where 
        (fst, scd) = span (/= ';') s
        last = dropWhile (==' ') (drop 1 scd)

-- Creates a If Command from a String
readCommandIf :: String -> (Command a, String)
readCommandIf s = (Cond b ifCom elCom, rem)
    where 
        init            = dropNextWord s        -- Remove IF 
        (b, bRem)       = readBoolExpr init     -- Read boolean expression
        thenRem         = dropNextWord bRem     -- Remove THEN
        (ifCom, ifRem)  = readCommandSeq thenRem       -- Read if commands, stops when founds the ELSE
        eRem            = dropNextWord ifRem    -- Remove ELSE
        (elCom, elRem)  = readCommandSeq elRem         -- Read else commands, stops when founds END
        rem             = dropNextWord elRem    -- Remove END

readCommandWhile :: String -> (Command a , String)
readCommandWhile s
    where
        init            = dropNextWord s        -- Remove WHILE
        (b, bRem)       = readBoolExpr init     -- Read boolean expression
        wRem            = dropNextWord

-- Creates a Seq Command from a String and the selected function
readCommandSomSeq :: (String -> (Command a, String)) -> String -> (Command a, String)
readCommandSomSeq _ [] = (Seq [], [])
readCommandSomSeq f s  = (Seq (com:xs), last)
    where   (com, rem)      = f s
            (Seq xs, last)  = readCommandSeq rem


-- Creates a Seq Command from a String
readCommandSeq :: String -> (Command a, String)
readCommandSeq s
    | com == "INPUT"    = readCommandSomSeq readCommandInput s
    | com == "IF"       = readCommandSomSeq readCommandIf s
    | com == "WHILE"    = readCommandSomSeq readCommandWhile s
  {-  | com == "DO"       =
    | com == "PRINT"    =
    | com == "END"      = -}
    | otherwise         = (Seq [], [])
    where 
        com             = takeWhile(\x -> x /= ' ') s

-- Creates an AST from the input String
readCommand :: Num a => String -> Command a
readCommand s = fst
    where (fst, scd) = readCommandSeq s
    

main = 
    do
        h <- openFile "codiPractica.txt" ReadMode
        s <- hGetContents h
        putStrLn (show (readCommand s))
