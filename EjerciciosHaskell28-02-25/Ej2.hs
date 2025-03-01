module DobleFuncion where
    -- Función de orden superior
    funcionSuperior :: (a -> b) -> [a] -> [b]
    funcionSuperior f lista = map f lista

    -- Función para elevar al cubo un número
    cubo :: Int -> Int
    cubo x = x * x * x

    main :: IO ()
    main = do
        let numeros = [1, 2, 3, 4, 5]
        putStrLn $ "Lista original: " ++ show numeros
        putStrLn $ "Lista duplicada: " ++ show (funcionSuperior cubo numeros)