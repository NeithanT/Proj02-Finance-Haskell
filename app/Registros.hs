module Registros where

import Types

montoValido :: Double -> Bool
montoValido m = m > 0

descripcionValida :: String -> Bool
descripcionValida d = not (null d)

agregarRegistro :: RegistroFinanciero -> [RegistroFinanciero] -> Either String [RegistroFinanciero]
agregarRegistro nuevo registros
    | not (montoValido (monto nuevo))       = Left "El monto debe ser mayor a cero."
    | not (descripcionValida (descripcion nuevo)) = Left "La descripcion no puede estar vacia."
    | otherwise                             = Right (registros ++ [nuevo])
