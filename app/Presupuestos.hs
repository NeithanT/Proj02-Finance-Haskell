module Presupuestos where

import Types
import Data.Char (toLower)

normalizarTexto :: String -> String
normalizarTexto = map toLower

mismaCategoriaTexto :: String -> String -> Bool
mismaCategoriaTexto a b = normalizarTexto a == normalizarTexto b

tieneEtiquetaCategoria :: String -> RegistroFinanciero -> Bool
tieneEtiquetaCategoria categoriaBuscada registro =
    normalizarTexto categoriaBuscada `elem` map normalizarTexto (etiquetas registro)

buscarPresupuesto :: String -> [Presupuesto] -> Maybe Presupuesto
buscarPresupuesto _ [] = Nothing
buscarPresupuesto categoriaBuscada (p:ps)
    | mismaCategoriaTexto categoriaBuscada (categoriaPresupuesto p) = Just p
    | otherwise = buscarPresupuesto categoriaBuscada ps

agregarPresupuesto :: Presupuesto -> [Presupuesto] -> [Presupuesto]
agregarPresupuesto nuevo [] = [nuevo]
agregarPresupuesto nuevo (p:ps)
    | mismaCategoriaTexto (categoriaPresupuesto nuevo) (categoriaPresupuesto p) = p : ps
    | otherwise = p : agregarPresupuesto nuevo ps

actualizarPresupuesto :: String -> Double -> [Presupuesto] -> [Presupuesto]
actualizarPresupuesto _ _ [] = []
actualizarPresupuesto categoriaBuscada nuevoMonto (p:ps)
    | mismaCategoriaTexto categoriaBuscada (categoriaPresupuesto p) =
        Presupuesto (categoriaPresupuesto p) nuevoMonto : ps
    | otherwise = p : actualizarPresupuesto categoriaBuscada nuevoMonto ps

eliminarPresupuesto :: String -> [Presupuesto] -> [Presupuesto]
eliminarPresupuesto _ [] = []
eliminarPresupuesto categoriaBuscada (p:ps)
    | mismaCategoriaTexto categoriaBuscada (categoriaPresupuesto p) = ps
    | otherwise = p : eliminarPresupuesto categoriaBuscada ps

gastoRealCategoria :: String -> [RegistroFinanciero] -> Double
gastoRealCategoria _ [] = 0
gastoRealCategoria categoriaBuscada (r:rs)
    | categoria r == Gasto && tieneEtiquetaCategoria categoriaBuscada r =
        monto r + gastoRealCategoria categoriaBuscada rs
    | otherwise = gastoRealCategoria categoriaBuscada rs

compararPresupuesto :: Presupuesto -> [RegistroFinanciero] -> (Double, Double)
compararPresupuesto presupuesto registros =
    (gastoReal, montoPresupuesto presupuesto)
  where
    gastoReal = gastoRealCategoria (categoriaPresupuesto presupuesto) registros

diferenciaPresupuesto :: Presupuesto -> [RegistroFinanciero] -> Double
diferenciaPresupuesto presupuesto registros =
    montoPresupuesto presupuesto - gastoRealCategoria (categoriaPresupuesto presupuesto) registros

excedioPresupuesto :: Presupuesto -> [RegistroFinanciero] -> Bool
excedioPresupuesto presupuesto registros =
    gastoRealCategoria (categoriaPresupuesto presupuesto) registros > montoPresupuesto presupuesto

alertaPresupuesto :: Presupuesto -> [RegistroFinanciero] -> String
alertaPresupuesto presupuesto registros
    | excedioPresupuesto presupuesto registros =
        "ALERTA: se excedio el presupuesto de " ++ categoriaPresupuesto presupuesto
    | otherwise =
        "OK: el presupuesto de " ++ categoriaPresupuesto presupuesto ++ " sigue en control"

resumenPresupuesto :: Presupuesto -> [RegistroFinanciero] -> String
resumenPresupuesto presupuesto registros =
    "Categoria: " ++ categoriaPresupuesto presupuesto
    ++ " | Gastado: " ++ show gastoReal
    ++ " | Presupuesto: " ++ show montoMaximo
    ++ " | Diferencia: " ++ show diferencia
  where
    gastoReal = gastoRealCategoria (categoriaPresupuesto presupuesto) registros
    montoMaximo = montoPresupuesto presupuesto
    diferencia = montoMaximo - gastoReal

resumenTodosPresupuestos :: [Presupuesto] -> [RegistroFinanciero] -> [String]
resumenTodosPresupuestos [] _ = []
resumenTodosPresupuestos (p:ps) registros =
    resumenPresupuesto p registros : resumenTodosPresupuestos ps registros