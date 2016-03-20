data BoolExpr a = 
    AND (BoolExpr a) (BoolExpr a) | 
    OR (BoolExpr a) (BoolExpr a) | 
    NOT (BoolExpr a) |
    Gt (NumExpr a) (NumExpr a) |
    Eq (NumExpr a) (NumExpr a)
    deriving(Show)

data NumExpr a = 
    Var a | 
    Const a |
    Plus (NumExpr a) (NumExpr a) |
    Minus (NumExpr a) (NumExpr a) |
    Times (NumExpr a) (NumExpr a) |
    Div (NumExpr a) (NumExpr a) 
    deriving(Show)

data Command a = 
    Assign a | 
    Input a | 
    Print a | 
    Seq a [Command a] | 
    Cond (BoolExpr a) (Command a) (Command a) | 
    Loop a (BoolExpr a) (Command a)
    deriving(Show)

readCommand :: Num a => String -> Command a
readCommand s = Assign 3