module Solucion where

-- Suma de elementos de una lista
sumarLista :: [Int] -> Int
sumarLista = sum

-- Factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n*factorial(n-1)

-- Números pares
numerosPares :: Int -> [Int]
numerosPares n=[x | x <- [1..n], even x]

-- Longitud de una lista
longitudCadena :: String -> Int
longitudCadena = length

-- Reverso de una cadena
reversoLista :: [a] -> [a]
reversoLista = reverse

-- Duplicar elementos
duplicarElementos :: [Int] -> [Int]
duplicarElementos = concatMap(\x -> [x, x])

-- Filtrar elementos pares
filtrarPares :: [Int]->[Int]
filtrarPares = filter even

-- Fibonacci
fibonacci :: Int->Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1)+fibonacci(n-2)

-- Divisores de un numero
divisores :: Int->[Int]
divisores n=[x | x <- [1..n], n `mod` x == 0]

-- Palindromo
esPalindromo :: String -> Bool
esPalindromo s = s == reverse s