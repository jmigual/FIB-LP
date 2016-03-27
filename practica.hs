import System.IO
import Data.List.Split
import Data.Maybe
import Text.Read

---------------------- MAIN CODE ---------------------

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
    show (Var s)        = '"':(s ++ "\"")
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
        showIdent n i (Cond b ii ee) = "IF " ++ (show b) ++ " THEN\n" ++ showIf ++ elseS ++ showEl 
            where 
                showIf      = showIdent (n+4) (n+4) ii
                showEl      = showIdent (n+4) (n+4) ee
                elseS       = '\n':(addSpaces n "ELSE\n")
        showIdent n i (Loop b l) = "WHILE " ++ (show b) ++ doS ++ loop ++ endS
            where
                doS         = '\n':(addSpaces n "DO\n")
                endS        = '\n':(addSpaces n "END\n")
                loop        = showIdent (n+4) (n+4) l
        showIdent n i c
            | i == 0        = show c
            | otherwise     = ' ':(showIdent n (i-1) c)

        -- Adds n spaces
        addSpaces :: Int -> String -> String
        addSpaces n s
            | n == 0        = s
            | otherwise     = ' ':(addSpaces (n-1) s)

-- Separates the next word from the passed String
dropNextWord ::  String -> (String, String)
dropNextWord s  = (w, d)
    where 
        begin       = dropWhile (\x -> x ==' ' || x == '\n') s
        (w, r)      = span (/=' ') begin
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
    | isJust num    = Const n
    | otherwise     = Var s
    where 
        num     = readMaybe s
        Just n  = num

-- Read multiplications, 4th level of recursion (last)
readNumExprMul :: Read a => String -> NumExpr a
readNumExprMul s    = concatMul nums
    where
        timesS      = splitOn " * " s
        nums        = map readStringNum timesS

        concatMul :: [NumExpr a] -> NumExpr a
        concatMul []        = error "concatMul: Empty list"
        concatMul (x:[])    = x
        concatMul (x:xs)    = Times x (concatMul xs)

-- Read divisions, 3rd level of recursion
readNumExprDiv :: Read a => String -> NumExpr a
readNumExprDiv s     = concatDiv nums
    where
        divsS   = splitOn " / " s
        nums    = map readNumExprMul divsS

        concatDiv :: [NumExpr a] -> NumExpr a
        concatDiv []        = error "concatDiv: Empty list"
        concatDiv (x:[])    = x
        concatDiv (x:xs)    = Div x (concatDiv xs)

-- Read minus, 2nd level of recursion
readNumExprMin :: Read a => String -> NumExpr a
readNumExprMin s    = concatMin nums
    where
        minsS   = splitOn " - " s
        nums    = map readNumExprDiv minsS

        concatMin :: [NumExpr a] -> NumExpr a
        concatMin []        = error "concatMin: Empty list"
        concatMin (x:[])    = x
        concatMin (x:xs)    = Minus x (concatMin xs)

-- Read plus, 1st level of recursion
readNumExprPlu :: Read a => String -> NumExpr a
readNumExprPlu s    = concatPlu nums
    where
        plusS   = splitOn " + " s
        nums    = map readNumExprMin plusS

        concatPlu :: [NumExpr a] -> NumExpr a
        concatPlu []        = error "concatPlu: Empty list"
        concatPlu (x:[])    = x
        concatPlu (x:xs)    = Plus x (concatPlu xs)

-- Reads a numeric expression and returns the remaining string
readNumExpr :: Read a => String -> (NumExpr a, String)
readNumExpr s = (readNumExprPlu str, r)
    where 
        (str, r)  = dropNextLine s--takeCommand s [">", "=", "THEN", "DO", "END"]

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

readBoolNot :: Read a => String -> BoolExpr a
readBoolNot s
    | com == "NOT"  = NOT (readBoolCom remaining)
    | otherwise     = readBoolCom s
    where 
        (com, remaining)    = dropNextWord s

readBoolAnd :: Read a => String -> BoolExpr a
readBoolAnd s = concatAnd bool
    where
        subs    = splitOn " AND " s
        bool    = map readBoolNot subs

        concatAnd :: [BoolExpr a] -> BoolExpr a
        concatAnd []        = error "concatAnd: Empty List"
        concatAnd (x:[])    = x
        concatAnd (x:xs)    = AND x (concatAnd xs)

readBoolOr :: Read a => String -> BoolExpr a
readBoolOr s = concatOr bool
    where
        subs    = splitOn " OR " s
        bool    = map readBoolAnd subs

        concatOr :: [BoolExpr a] -> BoolExpr a
        concatOr []     = error "concatOr: Empty List"
        concatOr (x:[]) = x
        concatOr (x:xs) = OR x (concatOr xs)

readBool :: Read a => String -> (BoolExpr a, String)
readBool s = (readBoolOr expr, dirty)
    where
        (expr, dirty)       = takeCommand s ["THEN", "DO"]

-- Creates a Input Command from a String
readCommandInput :: String -> (Command a, String)
readCommandInput s  = (Input (Var var), rema)
    where
        varDirty    = snd $ dropNextWord s              -- Remove INPUT
        (var, rema) = dropNextLine varDirty             -- Separate variable name

-- Creates a Print Command from a String
readCommandPrint :: Read a => String -> (Command a, String)
readCommandPrint s  = (Print (Var var), rema)
    where
        varDirty    = snd $ dropNextWord s      -- Remove PRINT
        (var, rema) = dropNextLine varDirty     -- Separate variable name

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
        (expr, remain) = readNumExpr dExpr        -- Get expression and next line

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
    
main :: IO ()
main = 
    do
        h <- openFile "codiProva.txt" ReadMode
        s <- hGetContents h
        putStrLn (show (readCommand s :: Command Int))
