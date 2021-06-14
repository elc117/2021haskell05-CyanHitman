-- 5a Pratica Haskell
-- Nome Gabriel da Silva França

import Text.Printf

--1
bmi :: Float -> Float -> String
bmi peso altura = 
    let imc = peso / (altura * altura)
    in 
      if imc <= 18.5 then "ABAIXO"
      else if imc >= 30 then "ACIMA"
      else
           "NORMAL"

--2
bmi' :: Float -> Float -> String
bmi' peso altura
    | imc <= 18.5 = "ABAIXO"
    | imc >= 30 = "ACIMA"
    | otherwise = "NORMAL"
    where imc = peso / (altura * altura)


--3
cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
  let expr = (sum $ zipWith (*) digits mults) `mod` 11
    in if expr < 2 then 0 else 11-expr

--4
andTable :: [(Bool, Bool, Bool)]
andTable = [(x,y,z) | x <- [True, False], y <- [True, False], z <- [x == True && y == True]]