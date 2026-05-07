module Reglas where

import Types
import Presupuestos (gastoRealCategoria)

condicionValida :: String -> Bool
condicionValida op =
    op == ">" || op == "<" || op == ">=" || op == "<=" || op == "==" || op == "="

compararSegunCondicion :: String -> Double -> Double -> Bool
compararSegunCondicion ">"  valorReal valorObjetivo = valorReal > valorObjetivo
compararSegunCondicion "<"  valorReal valorObjetivo = valorReal < valorObjetivo
compararSegunCondicion ">=" valorReal valorObjetivo = valorReal >= valorObjetivo
compararSegunCondicion "<=" valorReal valorObjetivo = valorReal <= valorObjetivo
compararSegunCondicion "==" valorReal valorObjetivo = valorReal == valorObjetivo
compararSegunCondicion "="  valorReal valorObjetivo = valorReal == valorObjetivo
compararSegunCondicion _    _         _             = False

valorActualRegla :: Regla -> [RegistroFinanciero] -> Double
valorActualRegla regla registros =
    gastoRealCategoria (presupuestoRelacionado regla) registros

evaluarRegla :: Regla -> [RegistroFinanciero] -> Bool
evaluarRegla regla registros
    | not (condicionValida (condicion regla)) = False
    | otherwise =
        compararSegunCondicion
            (condicion regla)
            (valorActualRegla regla registros)
            (valorAlerta regla)

generarMensajeRegla :: Regla -> [RegistroFinanciero] -> String
generarMensajeRegla regla registros
    | evaluarRegla regla registros =
        "ALERTA: " ++ mensajeAlerta regla
        ++ " | Categoria: " ++ presupuestoRelacionado regla
        ++ " | Valor actual: " ++ show valorReal
        ++ " | Condicion: " ++ condicion regla
        ++ " " ++ show (valorAlerta regla)
    | otherwise =
        "OK: regla no activada para "
        ++ presupuestoRelacionado regla
  where
    valorReal = valorActualRegla regla registros

reglasActivadas :: [Regla] -> [RegistroFinanciero] -> [Regla]
reglasActivadas [] _ = []
reglasActivadas (r:rs) registros
    | evaluarRegla r registros = r : reglasActivadas rs registros
    | otherwise = reglasActivadas rs registros

mensajesReglasActivadas :: [Regla] -> [RegistroFinanciero] -> [String]
mensajesReglasActivadas [] _ = []
mensajesReglasActivadas (r:rs) registros
    | evaluarRegla r registros = generarMensajeRegla r registros : mensajesReglasActivadas rs registros
    | otherwise = mensajesReglasActivadas rs registros

resumenRegla :: Regla -> [RegistroFinanciero] -> String
resumenRegla regla registros =
    "Categoria: " ++ presupuestoRelacionado regla
    ++ " | Valor actual: " ++ show valorReal
    ++ " | Condicion: " ++ condicion regla
    ++ " " ++ show (valorAlerta regla)
  where
    valorReal = valorActualRegla regla registros

resumenTodasLasReglas :: [Regla] -> [RegistroFinanciero] -> [String]
resumenTodasLasReglas [] _ = []
resumenTodasLasReglas (r:rs) registros =
    resumenRegla r registros : resumenTodasLasReglas rs registros