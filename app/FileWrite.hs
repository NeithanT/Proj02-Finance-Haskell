module FileWrite where
import Types
import System.IO

rutaRegistros :: FilePath
rutaRegistros = "app/data/registros.txt"

rutaReglas :: FilePath
rutaReglas = "app/data/reglas.txt"

rutaPresupuestos :: FilePath
rutaPresupuestos = "app/data/presupuestos.txt"

cargarRegistros :: IO [RegistroFinanciero]
cargarRegistros = do
    contents <- readFile rutaRegistros
    let ls = lines contents
    if null ls
       then return []
       else return (map read ls)

cargarReglas :: IO [Regla]
cargarReglas = do
    contents <- readFile rutaReglas
    let ls = lines contents
    if null ls
       then return []
       else return (map read ls)

cargarPresupuestos :: IO [Presupuesto]
cargarPresupuestos = do
    contents <- readFile rutaPresupuestos
    let ls = lines contents
    if null ls
       then return []
       else return (map read ls)

guardarRegistros :: [RegistroFinanciero] -> IO ()
guardarRegistros registros =
    withFile rutaRegistros WriteMode $ \handle ->
        mapM_ (hPutStrLn handle . show) registros

guardarReglas :: [Regla] -> IO ()
guardarReglas reglas =
    withFile rutaReglas WriteMode $ \handle ->
        mapM_ (hPutStrLn handle . show) reglas

guardarPresupuestos :: [Presupuesto] -> IO ()
guardarPresupuestos presupuestos =
    withFile rutaPresupuestos WriteMode $ \handle ->
        mapM_ (hPutStrLn handle . show) presupuestos

cargarDatos :: IO ([RegistroFinanciero], [Regla], [Presupuesto])
cargarDatos = do
    registros <- cargarRegistros
    reglas <- cargarReglas
    presupuestos <- cargarPresupuestos
    return (registros, reglas, presupuestos)