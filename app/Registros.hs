module Registros where

import Types
import Data.Time (utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Char (toLower)
import Data.List (nub, sortBy)

montoValido :: Double -> Bool
montoValido m = m > 0

descripcionValida :: String -> Bool
descripcionValida d = not (null d)

agregarRegistro :: RegistroFinanciero -> [RegistroFinanciero] -> Either String [RegistroFinanciero]
agregarRegistro nuevo registros
    | not (montoValido (monto nuevo)) = Left "El monto debe ser mayor a cero."
    | not (descripcionValida (descripcion nuevo)) = Left "La descripcion no puede estar vacia."
    | otherwise = Right (registros ++ [nuevo])

eliminarRegistro :: Int -> [RegistroFinanciero] -> Either String [RegistroFinanciero]
eliminarRegistro indice registros
    | null registros = Left "No hay registros para eliminar."
    | indice < 1 = Left "El indice debe ser mayor a cero."
    | indice > length registros = Left "El indice no existe en la lista."
    | otherwise = Right (take (indice - 1) registros ++ drop indice registros)

formatearRegistro :: Int -> RegistroFinanciero -> String
formatearRegistro i r =
    show i ++ ". [" ++ show (categoria r) ++ "] "
    ++ descripcion r
    ++ " | Monto: " ++ show (monto r)
    ++ " | Fecha: " ++ show (utctDay (fecha r))
    ++ " | Etiquetas: " ++ unwords (etiquetas r)

listarRegistros :: [RegistroFinanciero] -> [String]
listarRegistros [] = ["No hay registros disponibles."]
listarRegistros rs = zipWith formatearRegistro [1..] rs

normalizar :: String -> String
normalizar = map toLower

filtrarPorCategoria :: CategoriaRegistro -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorCategoria cat = filter (\r -> categoria r == cat)

filtrarPorEtiqueta :: String -> [RegistroFinanciero] -> [RegistroFinanciero]
filtrarPorEtiqueta etiqueta = filter (\r -> normalizar etiqueta `elem` map normalizar (etiquetas r))

-- 2.3 Tendencias de gasto

mesAnoDeRegistro :: RegistroFinanciero -> (Integer, Int)
mesAnoDeRegistro r = (ano, mes)
  where
    (ano, mes, _) = toGregorian (utctDay (fecha r))

mesesUnicos :: [RegistroFinanciero] -> [(Integer, Int)]
mesesUnicos rs = nub (map mesAnoDeRegistro rs)

gastoEtiquetaEnMes :: String -> (Integer, Int) -> [RegistroFinanciero] -> Double
gastoEtiquetaEnMes etiqueta mesAno rs =
    sum [monto r | r <- rs
                 , categoria r == Gasto
                 , normalizar etiqueta `elem` map normalizar (etiquetas r)
                 , mesAnoDeRegistro r == mesAno]

tendenciaGasto :: String -> [RegistroFinanciero] -> [((Integer, Int), Double)]
tendenciaGasto etiqueta rs =
    sortBy (\(m1, _) (m2, _) -> compare m1 m2)
    [(m, gastoEtiquetaEnMes etiqueta m rs) | m <- mesesUnicos rs]

formatearTendencia :: ((Integer, Int), Double) -> String
formatearTendencia ((ano, mes), total) =
    show mes ++ "/" ++ show ano ++ " | Gasto: " ++ show total

-- 2.3 Proyeccion de gastos basada en datos historicos

proyectarGasto :: String -> [RegistroFinanciero] -> Double
proyectarGasto etiqueta rs
    | null tendencia = 0
    | otherwise = totalGasto / fromIntegral (length tendencia)
  where
    tendencia = tendenciaGasto etiqueta rs
    totalGasto = sum (map snd tendencia)

formatearProyeccion :: String -> Double -> String
formatearProyeccion etiqueta proyeccion =
    "Proyeccion proximo mes para '" ++ etiqueta ++ "': " ++ show proyeccion

-- 2.4 Simulacion financiera

simularReduccion :: String -> Double -> [RegistroFinanciero] -> Double
simularReduccion etiqueta porcentaje rs
    | porcentaje < 0 || porcentaje > 100 = gastoActual
    | otherwise = gastoActual * (1 - porcentaje / 100)
  where
    gastoActual = proyectarGasto etiqueta rs

formatearSimulacion :: String -> Double -> Double -> String
formatearSimulacion etiqueta porcentaje gastoNuevo =
    "Simulacion '" ++ etiqueta ++ "' reduciendo " ++ show porcentaje
    ++ "% -> Gasto proyectado: " ++ show gastoNuevo

proyectarAhorro :: Double -> Int -> Double
proyectarAhorro ahorroPorMes meses
    | ahorroPorMes <= 0 || meses <= 0 = 0
    | otherwise = ahorroPorMes * fromIntegral meses

formatearProyeccionAhorro :: Double -> Int -> Double -> String
formatearProyeccionAhorro ahorroPorMes meses total =
    "Ahorrando " ++ show ahorroPorMes ++ " por mes durante "
    ++ show meses ++ " meses = " ++ show total
