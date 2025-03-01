module Compras where
    -- Función para aplicar descuento
    totalDescuento :: Float -> Float -> Float
    totalDescuento precio descuento = (100-descuento)*precio/100
    
    -- Funcion para aplicar IVA
    totalIva :: Float -> Float
    totalIva precio = precio+(16*precio/100)

    total :: [(Float,Float)] -> (Float -> Float -> Float) -> Float
    total cesta funcion = sum [funcion precio porcentaje | (precio, porcentaje) <- cesta]

    main :: IO ()
    main = do
        let cesta = [(100, 10), (200, 5), (50, 20)] -- Lista de (precio, porcentaje)
        putStrLn $ "Precio con descuento: " ++ show (total cesta totalDescuento)
        putStrLn $ "Precio con IVA: " ++ show (total cesta (\precio _ -> totalIva precio))