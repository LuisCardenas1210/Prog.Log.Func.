module Numeros where
    {-funcion para saber si el numero es primo o no-}
    numeros :: Int -> String
    numeros n = if esPrimo n 2 then "FizzBuzz" else numerosIngles n
      where
        esPrimo x d
            | x < 2 = False  -- No es primo
            | d * d > x = True  -- No encontramos divisores, es primo
            | x `mod` d == 0 = False  -- Encontramos divisor, no es primo
            | otherwise = esPrimo x (d + 1 + if d == 2 then 0 else 1)
            {- 
            si el divisor (d) es 2, se suma 1, para que el divisor pase a valer 3
            si el divisor no es 2, suma 2, de esta manera de evita dividir entre numeros pares
            los cuales ya fueron revisados en la primera iteración
            -}
    {-Funcion para escribir los numeros de 0 a 1 millon en ingles-}
    numerosIngles :: Int->String
    numerosIngles n
        | n<20 = unidades n
        | n<100 && n `mod` 10 == 0 = decenas (n `div` 10 )
        | n<100 = decenas (n `div` 10) ++ " " ++ unidades(n `mod` 10)
        | n<1000 && n `mod` 100 == 0 = unidades (n `div` 100) ++ " hundred" 
        | n<1000 = unidades (n `div` 100) ++ " hundred and " ++ numerosIngles (n `mod` 100)
        | n<1000000 && n `mod` 1000 == 0 = numerosIngles (n `div` 1000) ++ " thousand"
        | n<1000000 = numerosIngles (n `div` 1000) ++ " thousand " ++ numerosIngles(n `mod` 1000)
        | n==1000000 = "million"        
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