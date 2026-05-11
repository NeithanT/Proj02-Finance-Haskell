module Registros where

import Types
import Data.Time (utctDay)
import Data.Char (toLower)

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
