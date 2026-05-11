module Analisis where

import Types
import Reportes (registrosDelMes, totalPorTipo, flujoCajaMensual, categoriasConMayorGasto)

-- ============================================================
-- 2.3 Análisis financiero avanzado
-- ============================================================

-- Flujo de caja mensual (formateado)
flujoCajaMensualAnalisis :: Integer -> Int -> [RegistroFinanciero] -> String
flujoCajaMensualAnalisis anio mes registros =
    "Flujo de caja " ++ show mes ++ "/" ++ show anio
    ++ " | Ingresos: " ++ show ingresos
    ++ " | Gastos: " ++ show gastos
    ++ " | Ahorros: " ++ show ahorros
    ++ " | Inversiones: " ++ show inversiones
    ++ " | Flujo neto: " ++ show flujo
  where
    regs      = registrosDelMes anio mes registros
    ingresos  = totalPorTipo Ingreso regs
    gastos    = totalPorTipo Gasto regs
    ahorros   = totalPorTipo Ahorro regs
    inversiones = totalPorTipo Inversion regs
    flujo     = flujoCajaMensual anio mes registros

-- Total de gastos de un mes
gastoMensual :: Integer -> Int -> [RegistroFinanciero] -> Double
gastoMensual anio mes registros =
    totalPorTipo Gasto (registrosDelMes anio mes registros)

-- Tendencias de gasto: variación porcentual entre periodos consecutivos
tendenciaGasto :: [(Integer, Int)] -> [RegistroFinanciero] -> [String]
tendenciaGasto [] _ = []
tendenciaGasto [p] _ = []
tendenciaGasto (p1:p2:ps) registros =
    linea : tendenciaGasto (p2:ps) registros
  where
    g1 = gastoDelPeriodo p1 registros
    g2 = gastoDelPeriodo p2 registros
    variacion = calcularVariacion g1 g2
    linea = "De " ++ formatearPeriodo p1
        ++ " a " ++ formatearPeriodo p2
        ++ " | Gasto anterior: " ++ show g1
        ++ " | Gasto nuevo: " ++ show g2
        ++ " | Variacion: " ++ mostrarPorcentaje variacion

-- Helpers internos
gastoDelPeriodo :: (Integer, Int) -> [RegistroFinanciero] -> Double
gastoDelPeriodo (anio, mes) = gastoMensual anio mes

formatearPeriodo :: (Integer, Int) -> String
formatearPeriodo (anio, mes) = show mes ++ "/" ++ show anio

calcularVariacion :: Double -> Double -> Double
calcularVariacion anterior actual
    | anterior == 0 = 0
    | otherwise     = ((actual - anterior) / anterior) * 100

mostrarPorcentaje :: Double -> String
mostrarPorcentaje p =
    let signo = if p >= 0 then "+" else ""
        entero = round p :: Int
    in signo ++ show entero ++ "%"

-- ============================================================
-- Submenú interactivo de análisis
-- ============================================================

menuAnalisis :: [RegistroFinanciero] -> IO ()
menuAnalisis registros = do
    putStrLn ""
    putStrLn "--- ANALISIS FINANCIERO ---"
    putStrLn "1. Flujo de caja mensual"
    putStrLn "2. Tendencias de gasto"
    putStrLn "3. Proyeccion de gastos"
    putStrLn "4. Categorias con mayor impacto"
    putStrLn "5. Volver al menu principal"
    putStrLn "Seleccione una opcion:"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Digite el año:"
            anio <- readLn :: IO Integer
            putStrLn "Digite el mes (1-12):"
            mes <- readLn :: IO Int
            putStrLn (flujoCajaMensualAnalisis anio mes registros)
            menuAnalisis registros

        "2" -> do
            putStrLn "Digite la cantidad total de meses consecutivos a comparar:"
            n <- readLn :: IO Int
            putStrLn "Digite el año de inicio:"
            anioInicio <- readLn :: IO Integer
            putStrLn "Digite el mes de inicio (1-12):"
            mesInicio <- readLn :: IO Int
            let periodos = generarPeriodos anioInicio mesInicio n
            mapM_ putStrLn (tendenciaGasto periodos registros)
            menuAnalisis registros

        "3" -> do
            putStrLn "Cuantos meses historicos desea considerar?"
            nHist <- readLn :: IO Int
            putStrLn "Digite el año de inicio historico:"
            anioHist <- readLn :: IO Integer
            putStrLn "Digite el mes de inicio historico (1-12):"
            mesHist <- readLn :: IO Int
            putStrLn "Cuantos meses desea proyectar?"
            nProy <- readLn :: IO Int
            let periodos = generarPeriodos anioHist mesHist nHist
            mapM_ putStrLn (proyeccionGasto periodos nProy registros)
            menuAnalisis registros

        "4" -> do
            mapM_ putStrLn (impactoFinanciero registros)
            menuAnalisis registros

        "5" -> return ()

        _ -> do
            putStrLn "Opcion invalida."
            menuAnalisis registros

-- Genera una lista de N periodos (año, mes) a partir de un punto inicial
generarPeriodos :: Integer -> Int -> Int -> [(Integer, Int)]
generarPeriodos _ _ 0 = []
generarPeriodos anio numMes n =
    (anio, numMes) : generarPeriodos siguienteAnio siguienteMes (n - 1)
  where
    siguienteMes = if numMes == 12 then 1 else numMes + 1
    siguienteAnio = if numMes == 12 then anio + 1 else anio
