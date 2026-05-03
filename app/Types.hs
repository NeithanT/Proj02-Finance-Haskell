module Types where
import Data.Time (UTCTime)

--Ingresos Todos los Registros donde categoria es de tipo Ingreso
--Gastos Todos los Registros donde categoria es de tipo Gasto
--Ahorros Todos los Registros donde categoria es de tipo Ahorro (Estos restan, pero suman a una cuenta de ahorro)
--Inversiones Todos los Registros donde categoria es de tipo Inversion (Estos restan, pero suman a una cuenta de inversion)

-- Tipos auxiliares

data CategoriaRegistro = Ingreso | Gasto | Ahorro | Inversion
    deriving (Show, Eq, Read, Enum, Bounded)

-- Cada registro financiero debe incluir:
-- Monto
-- Categoría
-- Fecha
-- Descripción
-- Etiquetas múltiples (ej: “fijo”, “variable”)
-- Esto es como un movimiento

data RegistroFinanciero = RegistroFinanciero {
    monto :: Double,
    categoria :: CategoriaRegistro,
    fecha :: UTCTime,
    descripcion :: String,
    etiquetas :: [String]
} deriving (Show, Eq, Read)

-- Presupuestos
-- Definir presupuestos por categoría
-- Comparar registros financieros reales vs presupuesto
-- Generar alertas cuando se exceda

-- Si va a realizar una compra, debe entrar dentro de un presupuesto,
-- ya sea comida, ropa, etc
data Presupuesto = Presupuesto {
    categoriaPresupuesto :: String,
    montoPresupuesto :: Double
} deriving (Show, Eq, Read)


-- Análisis financiero avanzado
-- Flujo de caja mensual
-- Tendencias de gasto
-- Proyección de gastos basada en datos históricos
-- Identificación de categorías con mayor impacto financiero

-- Simulación financiera (Esto es lo mismo de arriba, pero diciendo, gastar 20% menos en comida o asi)
-- Simular escenarios como:
-- Reducción de gastos en cierto porcentaje
-- Proyección de ahorro en el tiempo

-- Sistema de reglas
-- El sistema debe permitir definir reglas como:
-- “Si los gastos en una categoría superan cierto monto → generar alerta”
-- “Si el ahorro es menor a cierto valor → advertencia”

-- consisten en menor que o mayor que
-- si el presupuesto usado supero los 500 alertar
-- si no se ha usado 300 de ahorro, alertar, etc
data Regla = Regla {
    condicion :: String, -- esto es un > o <, se puede cambiar
    valorAlerta :: Double,
    presupuestoRelacionado :: String, -- esto es para saber a que presupuesto se refiere, se puede cambiar
    mensajeAlerta :: String
} deriving (Show, Eq, Read)


-- Reportes

-- Resumen mensual
-- Comparación entre periodos
-- Categorías con mayor gasto

data Reporte = Reporte {
    mes :: UTCTime, -- podria cambiarse a String en un futuro, pero representa el mes del Reporte
    movimientos :: [RegistroFinanciero]
} deriving (Show, Eq, Read)