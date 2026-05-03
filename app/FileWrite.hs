module FileWrite where
import Types
import System.IO

-- Persistencia estructurada
-- Los registros financieros deben almacenarse en archivo (formato estructurado)
-- El sistema debe reconstruir la información correctamente al iniciar

-- Cargar los archivos

cargarRegistros :: IO [RegistroFinanciero]
cargarRegistros = do
    let filePath = "app/data/registros.txt"
    contents <- readFile filePath
    let registros = map read (lines contents) :: [RegistroFinanciero]
    return registros

cargarReglas :: IO [Regla]
cargarReglas = do
    let filePath = "app/data/reglas.txt"
    contents <- readFile filePath
    let reglas = map read (lines contents) :: [Regla]
    return reglas

cargarPresupuestos :: IO [Presupuesto]
cargarPresupuestos = do
    let filePath = "app/data/presupuestos.txt"
    contents <- readFile filePath
    let presupuestos = map read (lines contents) :: [Presupuesto]
    return presupuestos

cargarDatos :: IO ()
cargarDatos = do
    registros <- cargarRegistros
    reglas <- cargarReglas
    presupuestos <- cargarPresupuestos
    putStrLn "Datos cargados exitosamente"
guardarRegistros :: [RegistroFinanciero] -> IO ()
guardarRegistros registros = do
    let filePath = "registros.txt"
    withFile filePath WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle . show) registros

guardarReglas :: [Regla] -> IO ()
guardarReglas reglas = do
    let filePath = "reglas.txt"
    withFile filePath WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle . show) reglas

guardarPresupuestos :: [Presupuesto] -> IO ()
guardarPresupuestos presupuestos = do
    let filePath = "presupuestos.txt"
    withFile filePath WriteMode $ \handle -> do
        mapM_ (hPutStrLn handle . show) presupuestos