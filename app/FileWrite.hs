module FileWrite where

import Types
import System.IO
import Data.Char (isSpace)
import Text.Read (readMaybe)

rutaRegistros :: FilePath
rutaRegistros = "app/data/registros.txt"

rutaReglas :: FilePath
rutaReglas = "app/data/reglas.txt"

rutaPresupuestos :: FilePath
rutaPresupuestos = "app/data/presupuestos.txt"

limpiarLinea :: String -> String
limpiarLinea = quitarEspaciosFinales . quitarEspaciosIniciales
  where
    quitarEspaciosIniciales = dropWhile isSpace
    quitarEspaciosFinales = reverse . dropWhile isSpace . reverse

lineasValidas :: String -> [String]
lineasValidas contenido =
    filter (not . null) (map limpiarLinea (lines contenido))

leerListaDesdeArchivo :: Read a => FilePath -> IO [a]
leerListaDesdeArchivo ruta = do
    contenido <- readFile ruta
    let ls = lineasValidas contenido
    case traverse readMaybe ls of
        Just xs -> return xs
        Nothing -> error ("Error al leer datos en el archivo: " ++ ruta)

cargarRegistros :: IO [RegistroFinanciero]
cargarRegistros = leerListaDesdeArchivo rutaRegistros

cargarReglas :: IO [Regla]
cargarReglas = leerListaDesdeArchivo rutaReglas

cargarPresupuestos :: IO [Presupuesto]
cargarPresupuestos = leerListaDesdeArchivo rutaPresupuestos

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