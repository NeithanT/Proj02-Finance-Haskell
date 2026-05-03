module Main where
import FileWrite
import Types

main :: IO ()
main = do
    registros <- cargarRegistros
    reglas <- cargarReglas
    presupuestos <- cargarPresupuestos
    putStrLn "+------ Bienvenido al Proyecto de Gestion Financiera ------+"