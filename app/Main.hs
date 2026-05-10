A module Main where

import FileWrite
import Presupuestos
import Reglas
import Reportes

mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn ""
    putStrLn "----------- MENU PRINCIPAL -----------"
    putStrLn "1. Ver resumen de presupuestos"
    putStrLn "2. Ver reglas activadas"
    putStrLn "3. Ver top 3 categorias con mayor gasto"
    putStrLn "4. Ver resumen mensual"
    putStrLn "5. Salir"
    putStrLn "--------------------------------------"
    putStrLn "Digite una opcion:"

main :: IO ()
main = do
    putStrLn "+------ Bienvenido al Proyecto de Gestion Financiera ------+"
    cicloPrincipal

cicloPrincipal :: IO ()
cicloPrincipal = do
    (registros, reglas, presupuestos) <- cargarDatos
    mostrarMenu
    opcion <- getLine

    case opcion of
        "1" -> do
            if null presupuestos
               then putStrLn "No hay presupuestos registrados."
               else mapM_ putStrLn (resumenTodosPresupuestos presupuestos registros)
            cicloPrincipal

        "2" -> do
            if null reglas
               then putStrLn "No hay reglas registradas."
               else do
                   let mensajes = mensajesReglasActivadas reglas registros
                   if null mensajes
                      then putStrLn "No hay reglas activadas en este momento."
                      else mapM_ putStrLn mensajes
            cicloPrincipal

        "3" -> do
            let reporte = reporteTopCategorias 3 registros
            if null reporte
               then putStrLn "No hay datos de gastos para generar el reporte."
               else mapM_ putStrLn reporte
            cicloPrincipal

        "4" -> do
            putStrLn "Digite el anio:"
            anio <- readLn :: IO Integer
            putStrLn "Digite el mes (1-12):"
            mes <- readLn :: IO Int
            putStrLn (resumenMensual anio mes registros)
            cicloPrincipal

        "5" -> do
            putStrLn "Saliendo del sistema..."

        _ -> do
            putStrLn "Opcion invalida."
            cicloPrincipal