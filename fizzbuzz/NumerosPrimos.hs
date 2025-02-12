module Numeros where
    {-funcion para saber si el numero es primo o no-}
    numeros :: Int -> String
    numeros n = if esPrimo n 2 then "FizzBuzz" else numerosEspaniol n
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
    {-Funcion para escribir los numeros de 0 a 1 millon en espaniol-}
    numerosEspaniol :: Int->String
    numerosEspaniol n
        | n<20 = unidades n
        | n<100 && n `mod` 10 == 0 = decenas (n `div` 10 )
        | n<100 = decenas (n `div` 10) ++ " y " ++ unidades(n `mod` 10)
        | n==100 = "cien"
        | n<1000 && n `mod` 100 == 0 = centenas (n `div` 100) 
        | n<1000 = centenas (n `div` 100) ++ " " ++ numerosEspaniol (n `mod` 100)
        | n==1000 = "mil"
        | n<2000 = "mil " ++ numerosEspaniol (n `mod` 1000)
        | n<1000000 && n `mod` 1000 == 0 = numerosEspaniol (n `div` 1000) ++ " mil"
        | n<1000000 = numerosEspaniol (n `div` 1000) ++ " mil " ++ numerosEspaniol(n `mod` 1000)
        | n==1000000 = "millon"        
      where 
        unidades x = case x of
            0 -> "cero";  1 -> "uno";   2 -> "dos";   3 -> "tres";  4 -> "cuatro"
            5 -> "cinco";  6 -> "seis";   7 -> "siete"; 8 -> "ocho";  9 -> "nueve"
            10 -> "diez";  11 -> "once";  12 -> "doce";  13 -> "nueve"; 14 -> "catorce"
            15 -> "quince"; 16 -> "dieciseis"; 17-> "diecisiete"; 18-> "dieciocho"; 19-> "diecinueve"
        
        decenas x = case x of
            1-> "diez"; 2 -> "veinte"; 3 -> "treinta"; 4 -> "cuarenta"; 5 -> "cincuenta"
            6 -> "sesenta"; 7 -> "setenta"; 8 -> "ochenta"; 9 -> "noventa"

        centenas x = case x of
            1-> "ciento"; 2 -> "docientos"; 3 -> "trecientos"; 4 -> "cuatrocientos"; 5 -> "quinientos"
            6 -> "seicientos"; 7 -> "setecientos"; 8 -> "ococientos"; 9 -> "novecientos"