module Main where
import FileWrite
import Types

main :: IO ()
main = do
    (registros, reglas, presupuestos) <- cargarDatos
    putStrLn "+------ Bienvenido al Proyecto de Gestion Financiera ------+"
    putStrLn ("Registros cargados: " ++ show (length registros))
    putStrLn ("Reglas cargadas: " ++ show (length reglas))
    putStrLn ("Presupuestos cargados: " ++ show (length presupuestos))