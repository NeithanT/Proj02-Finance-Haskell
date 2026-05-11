module Simulacion where

import Types
import Reportes (totalPorTipo)

-- ============================================================
-- 2.4 Simulación financiera
-- ============================================================

-- Total general de gastos (sin filtrar por mes)
totalGastos :: [RegistroFinanciero] -> Double
totalGastos = totalPorTipo Gasto

-- Total general de ingresos
totalIngresos :: [RegistroFinanciero] -> Double
totalIngresos = totalPorTipo Ingreso

-- Simular reducción de gastos en un porcentaje dado
-- Ejemplo: reducir 20% => porcentaje = 20
simularReduccionGastos :: Double -> [RegistroFinanciero] -> Double
simularReduccionGastos porcentaje registros =
    gastos * (1 - porcentaje / 100)
  where
    gastos = totalGastos registros

-- Cuánto se ahorra al reducir gastos en cierto porcentaje
ahorroPorReduccion :: Double -> [RegistroFinanciero] -> Double
ahorroPorReduccion porcentaje registros =
    gastos * (porcentaje / 100)
  where
    gastos = totalGastos registros

-- Proyección de ahorro simple: ahorro mensual * N meses
proyeccionAhorro :: Double -> Int -> Double
proyeccionAhorro ahorroMensual meses =
    ahorroMensual * fromIntegral meses

-- Proyección de ahorro con interés compuesto mensual
-- tasaMensual en porcentaje, ej: 1.5 = 1.5%
proyeccionAhorroConInteres :: Double -> Double -> Int -> Double
proyeccionAhorroConInteres ahorroMensual tasaMensual meses =
    calcularConInteres ahorroMensual (tasaMensual / 100) meses 0

calcularConInteres :: Double -> Double -> Int -> Double -> Double
calcularConInteres _ _ 0 acumulado = acumulado
calcularConInteres aporte tasa mesRestante acumulado =
    calcularConInteres aporte tasa (mesRestante - 1) nuevoAcumulado
  where
    nuevoAcumulado = (acumulado + aporte) * (1 + tasa)

-- Reporte completo de simulación
simularEscenarioCompleto :: Double -> Int -> [RegistroFinanciero] -> [String]
simularEscenarioCompleto porcentaje meses registros =
    [ "--- Simulacion Financiera ---"
    , "Gastos actuales totales:    " ++ show gastos
    , "Reduccion aplicada:         " ++ show porcentaje ++ "%"
    , "Gastos simulados:           " ++ show gastosReducidos
    , "Ahorro mensual estimado:    " ++ show ahorroMensual
    , "Proyeccion a " ++ show meses ++ " meses:     " ++ show proyeccion
    ]
  where
    gastos          = totalGastos registros
    gastosReducidos = simularReduccionGastos porcentaje registros
    ahorroMensual   = ahorroPorReduccion porcentaje registros
    proyeccion      = proyeccionAhorro ahorroMensual meses

-- Reporte de proyección de ahorro detallado mes a mes
reporteProyeccionAhorro :: Double -> Int -> [String]
reporteProyeccionAhorro ahorroMensual meses =
    "--- Proyeccion de Ahorro ---"
    : ("Ahorro mensual: " ++ show ahorroMensual)
    : generarDetalleAhorro 1 meses ahorroMensual

generarDetalleAhorro :: Int -> Int -> Double -> [String]
generarDetalleAhorro actual total ahorroMensual
    | actual > total = []
    | otherwise =
        ("  Mes " ++ show actual
         ++ ": acumulado = " ++ show (ahorroMensual * fromIntegral actual))
        : generarDetalleAhorro (actual + 1) total ahorroMensual

-- ============================================================
-- Submenú interactivo de simulación
-- ============================================================

menuSimulacion :: [RegistroFinanciero] -> IO ()
menuSimulacion registros = do
    putStrLn ""
    putStrLn "--- SIMULACION FINANCIERA ---"
    putStrLn "1. Simular reduccion de gastos"
    putStrLn "2. Proyeccion de ahorro en el tiempo"
    putStrLn "3. Simulacion completa (reduccion + proyeccion)"
    putStrLn "4. Volver al menu principal"
    putStrLn "Seleccione una opcion:"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "En que porcentaje desea reducir gastos? (ej: 20 para 20%)"
            porcentaje <- readLn :: IO Double
            let gastos = totalGastos registros
            let reducidos = simularReduccionGastos porcentaje registros
            let ahorro = ahorroPorReduccion porcentaje registros
            putStrLn ("Gastos actuales: " ++ show gastos)
            putStrLn ("Gastos con reduccion del " ++ show porcentaje ++ "%: " ++ show reducidos)
            putStrLn ("Ahorro generado: " ++ show ahorro)
            menuSimulacion registros

        "2" -> do
            putStrLn "Cual es el monto de ahorro mensual?"
            ahorroMensual <- readLn :: IO Double
            putStrLn "A cuantos meses desea proyectar?"
            meses <- readLn :: IO Int
            mapM_ putStrLn (reporteProyeccionAhorro ahorroMensual meses)
            menuSimulacion registros

        "3" -> do
            putStrLn "En que porcentaje desea reducir gastos? (ej: 15 para 15%)"
            porcentaje <- readLn :: IO Double
            putStrLn "A cuantos meses desea proyectar?"
            meses <- readLn :: IO Int
            mapM_ putStrLn (simularEscenarioCompleto porcentaje meses registros)
            menuSimulacion registros

        "4" -> return ()

        _ -> do
            putStrLn "Opcion invalida."
            menuSimulacion registros
