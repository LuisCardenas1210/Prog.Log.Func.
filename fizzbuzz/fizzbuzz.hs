module Main where
{-
Funcion fizzbuzz:
verifica si el numero ingresado es multiplo de 3, 5 o ambos con la funcion `mod`
si no es multiplo, entra a la funcion numeros
-}
fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15 == 0 = "FizzBuzz!"
    | n `mod` 5 == 0 = "Fizz!"
    | n `mod` 3 == 0 = "Buzz!"
    | otherwise = numeros n
{-
Funcion numeros:
transforma el numero ingresado, en un string del numero en ingles
si es menor a 20, escribe el numero asignado, si es mayor, concatena 
las decenas y las unidades
-}
numeros :: Int -> String
numeros n
    | n < 20 = unidades n
    | n `mod` 10 == 0 = decenas (n `div` 10)
    | otherwise = decenas (n `div` 10) ++ " " ++ unidades (n `mod` 10)
  where
    unidades x = case x of
        0 -> "zero";  1 -> "one";   2 -> "two";   3 -> "three";  4 -> "four"
        5 -> "five";  6 -> "six";   7 -> "seven"; 8 -> "eight";  9 -> "nine"
        10 -> "ten";  11 -> "eleven";  12 -> "twelve";  13 -> "thirteen"; 14 -> "fourteen"
        15 -> "fifteen"; 16 -> "sixteen"; 17 -> "seventeen"; 18 -> "eighteen"; 19 -> "nineteen"
    
    decenas x = case x of
        2 -> "twenty"; 3 -> "thirty"; 4 -> "forty"; 5 -> "fifty"
        6 -> "sixty"; 7 -> "seventy"; 8 -> "eighty"; 9 -> "ninety"
        _ -> ""
{-
la función main muestra el mensaje "ingresa un numero del 1 al 100"
y no manda a la función fizzbuzz
-}
main :: IO ()
main = do
    putStrLn "Introduce un número entre 0 y 100:"
    input <- getLine
    let num = read input :: Int
    if num >= 0 && num <= 100
        then putStrLn $ fizzbuzz num
        else putStrLn "Número fuera del rango permitido"
