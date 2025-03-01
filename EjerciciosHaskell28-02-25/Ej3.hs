import Data.Map (Map, fromListWith)

-- Función que recibe una frase y devuelve un diccionario (Map) con las palabras y su longitud
contarLongitudes :: String -> Map String Int
contarLongitudes frase = fromListWith (+) [(palabra, length palabra) | palabra <- words frase]

-- Ejemplo de uso
main :: IO ()
main = do
    let frase = "Inserta una frase aqui"
    print $ contarLongitudes frase
