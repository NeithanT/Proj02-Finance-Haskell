module Reportes where

import Types
import Data.Time (UTCTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.List (nub, sortBy)
import Data.Char (toLower)

normalizarTexto :: String -> String
normalizarTexto = map toLower

mismoMesYAnio :: Integer -> Int -> UTCTime -> Bool
mismoMesYAnio anioBuscado mesBuscado fechaRegistro =
    anioRegistro == anioBuscado && mesRegistro == mesBuscado
  where
    (anioRegistro, mesRegistro, _) = toGregorian (utctDay fechaRegistro)

registrosDelMes :: Integer -> Int -> [RegistroFinanciero] -> [RegistroFinanciero]
registrosDelMes _ _ [] = []
registrosDelMes anio mes (r:rs)
    | mismoMesYAnio anio mes (fecha r) = r : registrosDelMes anio mes rs
    | otherwise = registrosDelMes anio mes rs

totalPorTipo :: CategoriaRegistro -> [RegistroFinanciero] -> Double
totalPorTipo _ [] = 0
totalPorTipo tipoBuscado (r:rs)
    | categoria r == tipoBuscado = monto r + totalPorTipo tipoBuscado rs
    | otherwise = totalPorTipo tipoBuscado rs

totalIngresosMes :: Integer -> Int -> [RegistroFinanciero] -> Double
totalIngresosMes anio mes registros =
    totalPorTipo Ingreso (registrosDelMes anio mes registros)

totalGastosMes :: Integer -> Int -> [RegistroFinanciero] -> Double
totalGastosMes anio mes registros =
    totalPorTipo Gasto (registrosDelMes anio mes registros)

totalAhorrosMes :: Integer -> Int -> [RegistroFinanciero] -> Double
totalAhorrosMes anio mes registros =
    totalPorTipo Ahorro (registrosDelMes anio mes registros)

totalInversionesMes :: Integer -> Int -> [RegistroFinanciero] -> Double
totalInversionesMes anio mes registros =
    totalPorTipo Inversion (registrosDelMes anio mes registros)

flujoCajaMensual :: Integer -> Int -> [RegistroFinanciero] -> Double
flujoCajaMensual anio mes registros =
    ingresos - gastos - ahorros - inversiones
  where
    ingresos = totalIngresosMes anio mes registros
    gastos = totalGastosMes anio mes registros
    ahorros = totalAhorrosMes anio mes registros
    inversiones = totalInversionesMes anio mes registros

resumenMensual :: Integer -> Int -> [RegistroFinanciero] -> String
resumenMensual anio mes registros =
    "Resumen " ++ show mes ++ "/" ++ show anio
    ++ " | Ingresos: " ++ show ingresos
    ++ " | Gastos: " ++ show gastos
    ++ " | Ahorros: " ++ show ahorros
    ++ " | Inversiones: " ++ show inversiones
    ++ " | Flujo neto: " ++ show flujo
  where
    ingresos = totalIngresosMes anio mes registros
    gastos = totalGastosMes anio mes registros
    ahorros = totalAhorrosMes anio mes registros
    inversiones = totalInversionesMes anio mes registros
    flujo = flujoCajaMensual anio mes registros

compararPeriodos :: (Integer, Int) -> (Integer, Int) -> [RegistroFinanciero] -> String
compararPeriodos (anio1, mes1) (anio2, mes2) registros =
    "Periodo 1 (" ++ show mes1 ++ "/" ++ show anio1 ++ ")"
    ++ " | Gastos: " ++ show gastos1
    ++ " | Flujo neto: " ++ show flujo1
    ++ " || Periodo 2 (" ++ show mes2 ++ "/" ++ show anio2 ++ ")"
    ++ " | Gastos: " ++ show gastos2
    ++ " | Flujo neto: " ++ show flujo2
    ++ " || Diferencia de gastos: " ++ show (gastos2 - gastos1)
  where
    gastos1 = totalGastosMes anio1 mes1 registros
    gastos2 = totalGastosMes anio2 mes2 registros
    flujo1 = flujoCajaMensual anio1 mes1 registros
    flujo2 = flujoCajaMensual anio2 mes2 registros

todasLasEtiquetasGasto :: [RegistroFinanciero] -> [String]
todasLasEtiquetasGasto [] = []
todasLasEtiquetasGasto (r:rs)
    | categoria r == Gasto = etiquetas r ++ todasLasEtiquetasGasto rs
    | otherwise = todasLasEtiquetasGasto rs

categoriasUnicasGasto :: [RegistroFinanciero] -> [String]
categoriasUnicasGasto registros =
    nub (map normalizarTexto (todasLasEtiquetasGasto registros))

gastoPorCategoriaTexto :: String -> [RegistroFinanciero] -> Double
gastoPorCategoriaTexto _ [] = 0
gastoPorCategoriaTexto categoriaBuscada (r:rs)
    | categoria r == Gasto
      && normalizarTexto categoriaBuscada `elem` map normalizarTexto (etiquetas r) =
        monto r + gastoPorCategoriaTexto categoriaBuscada rs
    | otherwise = gastoPorCategoriaTexto categoriaBuscada rs

construirResumenCategorias :: [String] -> [RegistroFinanciero] -> [(String, Double)]
construirResumenCategorias [] _ = []
construirResumenCategorias (c:cs) registros =
    (c, gastoPorCategoriaTexto c registros) : construirResumenCategorias cs registros

ordenarPorMontoDesc :: [(String, Double)] -> [(String, Double)]
ordenarPorMontoDesc =
    sortBy compararMontos
  where
    compararMontos (_, monto1) (_, monto2) = compare monto2 monto1

categoriasConMayorGasto :: [RegistroFinanciero] -> [(String, Double)]
categoriasConMayorGasto registros =
    ordenarPorMontoDesc (construirResumenCategorias (categoriasUnicasGasto registros) registros)

topCategoriasGasto :: Int -> [RegistroFinanciero] -> [(String, Double)]
topCategoriasGasto cantidad registros =
    take cantidad (categoriasConMayorGasto registros)

formatearCategoriaGasto :: (String, Double) -> String
formatearCategoriaGasto (categoriaTexto, totalGastado) =
    "Categoria: " ++ categoriaTexto ++ " | Total gastado: " ++ show totalGastado

reporteTopCategorias :: Int -> [RegistroFinanciero] -> [String]
reporteTopCategorias cantidad registros =
    map formatearCategoriaGasto (topCategoriasGasto cantidad registros)