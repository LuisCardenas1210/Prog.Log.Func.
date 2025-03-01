module ModuloVector where
-- Función que calcula el módulo de un vector
moduloVector :: [Double] -> Double
moduloVector vector = sqrt (sum (map (^2) vector))

-- Ejemplo de uso
main :: IO ()
main = do
    let vector = [5.0, 8.0]  -- Un vector en 2 dimensiones
    print $ moduloVector vector  -- Imprime el módulo del vector
