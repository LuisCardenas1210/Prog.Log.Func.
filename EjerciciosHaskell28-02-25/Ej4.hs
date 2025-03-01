import Data.Char (toUpper)  -- Importamos la función toUpper

-- Función que recibe un diccionario con asignaturas y notas y devuelve otro diccionario 
-- con las asignaturas en mayúsculas y las calificaciones correspondientes
asignaturas :: [(String, Int)] -> [(String, String)]
asignaturas = map (\(asignatura, nota) -> (map toUpper asignatura, calificacion nota))

-- Función que determina la calificación según la nota
calificacion :: Int -> String
calificacion nota
  | nota >= 95 = "Excelente"
  | nota >= 85 = "Notable"
  | nota >= 75 = "Bueno"
  | nota >= 70 = "Suficiente"
  | otherwise  = "Desempenio insuficiente"

-- Ejemplo de uso
main :: IO ()
main = do
    let diccionarioNotas = [("matematica", 92), ("historia", 68), ("fisica", 76), ("biologia", 83)]
    print $ asignaturas diccionarioNotas
