imc :: Float -> Float -> String
imc a b  
  | i < 18      = "magror"
  | i < 25      = "corpulencia normal"
  | i < 30      = "sobrepes"
  | i < 40      = "obesitat"
  | otherwise   = "obesitat morbida"
  where i = a / (b^2)
        

main = 
  do
    line <- getLine
    if line /= "*"
      then do
        let noms = (words line)
        let imi = imc (read (noms!!1)) (read (noms!!2))
        putStrLn $ (head noms) ++ ": " ++ imi
        main
      else return ()