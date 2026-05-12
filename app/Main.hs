module Main where

import FileWrite
import Presupuestos
import Reglas
import Registros
import Reportes
import Analisis
import Simulacion

import Types
import Data.Time (getCurrentTime)

-- MENUS

mostrarMenuPrincipal :: IO ()
mostrarMenuPrincipal = do
    putStrLn ""
    putStrLn "--------- MENU PRINCIPAL ---------"
    putStrLn "1. Gestion de Registros"
    putStrLn "2. Presupuestos"
    putStrLn "3. Reglas"
    putStrLn "4. Reportes"
    putStrLn "5. Analisis Financiero"
    putStrLn "6. Simulacion Financiera"
    putStrLn "7. Salir"
    putStrLn "----------------------------------"
    putStrLn "Opcion:"

mostrarMenuRegistros :: IO ()
mostrarMenuRegistros = do
    putStrLn ""
    putStrLn "--- Gestion de Registros ---"
    putStrLn "1. Ver todos los registros"
    putStrLn "2. Agregar registro"
    putStrLn "3. Eliminar registro"
    putStrLn "4. Filtrar por categoria"
    putStrLn "5. Filtrar por etiqueta"
    putStrLn "6. Volver"
    putStrLn "Opcion:"

mostrarMenuPresupuestos :: IO ()
mostrarMenuPresupuestos = do
    putStrLn ""
    putStrLn "--- Presupuestos ---"
    putStrLn "1. Ver resumen de presupuestos"
    putStrLn "2. Agregar presupuesto"
    putStrLn "3. Actualizar presupuesto"
    putStrLn "4. Eliminar presupuesto"
    putStrLn "5. Volver"
    putStrLn "Opcion:"

mostrarMenuReglas :: IO ()
mostrarMenuReglas = do
    putStrLn ""
    putStrLn "--- Reglas ---"
    putStrLn "1. Ver reglas activadas"
    putStrLn "2. Ver todas las reglas"
    putStrLn "3. Agregar regla"
    putStrLn "4. Eliminar regla"
    putStrLn "5. Volver"
    putStrLn "Opcion:"

mostrarMenuReportes :: IO ()
mostrarMenuReportes = do
    putStrLn ""
    putStrLn "--- Reportes ---"
    putStrLn "1. Resumen mensual"
    putStrLn "2. Comparar dos periodos"
    putStrLn "3. Top categorias con mayor gasto"
    putStrLn "4. Volver"
    putStrLn "Opcion:"

-- CICLO PRINCIPAL

main :: IO ()
main = do
    putStrLn "+------ Bienvenido al Proyecto de Gestion Financiera ------+"
    cicloPrincipal

cicloPrincipal :: IO ()
cicloPrincipal = do
    (registros, reglas, presupuestos) <- cargarDatos
    mostrarMenuPrincipal
    opcion <- getLine
    case opcion of
        "1" -> menuRegistros registros reglas presupuestos
        "2" -> menuPresupuestos registros reglas presupuestos
        "3" -> menuReglas registros reglas presupuestos
        "4" -> menuReportes registros reglas presupuestos
        "5" -> menuAnalisis registros >> cicloPrincipal
        "6" -> menuSimulacion registros >> cicloPrincipal
        "7" -> putStrLn "Saliendo del sistema..."
        _   -> do
            putStrLn "Opcion invalida."
            cicloPrincipal

-- MENU REGISTROS

parsearCategoria :: String -> CategoriaRegistro
parsearCategoria "1" = Ingreso
parsearCategoria "2" = Gasto
parsearCategoria "3" = Ahorro
parsearCategoria "4" = Inversion
parsearCategoria _   = Gasto

pedirRegistro :: IO RegistroFinanciero
pedirRegistro = do
    putStrLn "Monto:"
    m <- readLn :: IO Double
    putStrLn "Tipo (1=Ingreso, 2=Gasto, 3=Ahorro, 4=Inversion):"
    tipoOpcion <- getLine
    let cat = parsearCategoria tipoOpcion
    putStrLn "Descripcion:"
    desc <- getLine
    putStrLn "Etiquetas (separadas por espacio):"
    etiquetasInput <- getLine
    ahora <- getCurrentTime
    return RegistroFinanciero
        { monto = m
        , categoria = cat
        , fecha = ahora
        , descripcion = desc
        , etiquetas = words etiquetasInput
        }

menuRegistros :: [RegistroFinanciero] -> [Regla] -> [Presupuesto] -> IO ()
menuRegistros registros reglas presupuestos = do
    mostrarMenuRegistros
    opcion <- getLine
    case opcion of
        "1" -> do
            mapM_ putStrLn (listarRegistros registros)
            menuRegistros registros reglas presupuestos
        "2" -> do
            nuevo <- pedirRegistro
            case agregarRegistro nuevo registros of
                Left err -> putStrLn ("Error: " ++ err)
                Right nuevos -> do
                    guardarRegistros nuevos
                    putStrLn "Registro agregado."
            cicloPrincipal
        "3" -> do
            mapM_ putStrLn (listarRegistros registros)
            putStrLn "Numero del registro a eliminar:"
            idx <- readLn :: IO Int
            case eliminarRegistro idx registros of
                Left err -> putStrLn ("Error: " ++ err)
                Right nuevos -> do
                    guardarRegistros nuevos
                    putStrLn "Registro eliminado."
            cicloPrincipal
        "4" -> do
            putStrLn "Categoria (1=Ingreso, 2=Gasto, 3=Ahorro, 4=Inversion):"
            catOpcion <- getLine
            mapM_ putStrLn (listarRegistros (filtrarPorCategoria (parsearCategoria catOpcion) registros))
            menuRegistros registros reglas presupuestos
        "5" -> do
            putStrLn "Etiqueta a buscar:"
            etiqueta <- getLine
            mapM_ putStrLn (listarRegistros (filtrarPorEtiqueta etiqueta registros))
            menuRegistros registros reglas presupuestos
        "6" -> cicloPrincipal
        _   -> do
            putStrLn "Opcion invalida."
            menuRegistros registros reglas presupuestos

-- MENU PRESUPUESTOS

menuPresupuestos :: [RegistroFinanciero] -> [Regla] -> [Presupuesto] -> IO ()
menuPresupuestos registros reglas presupuestos = do
    mostrarMenuPresupuestos
    opcion <- getLine
    case opcion of
        "1" -> do
            if null presupuestos
                then putStrLn "No hay presupuestos registrados."
                else mapM_ putStrLn (resumenTodosPresupuestos presupuestos registros)
            menuPresupuestos registros reglas presupuestos
        "2" -> do
            putStrLn "Categoria del presupuesto:"
            cat <- getLine
            putStrLn "Monto maximo:"
            m <- readLn :: IO Double
            let nuevo = Presupuesto { categoriaPresupuesto = cat, montoPresupuesto = m }
            guardarPresupuestos (agregarPresupuesto nuevo presupuestos)
            putStrLn "Presupuesto agregado."
            cicloPrincipal
        "3" -> do
            putStrLn "Categoria a actualizar:"
            cat <- getLine
            putStrLn "Nuevo monto:"
            m <- readLn :: IO Double
            guardarPresupuestos (actualizarPresupuesto cat m presupuestos)
            putStrLn "Presupuesto actualizado."
            cicloPrincipal
        "4" -> do
            putStrLn "Categoria a eliminar:"
            cat <- getLine
            guardarPresupuestos (eliminarPresupuesto cat presupuestos)
            putStrLn "Presupuesto eliminado."
            cicloPrincipal
        "5" -> cicloPrincipal
        _   -> do
            putStrLn "Opcion invalida."
            menuPresupuestos registros reglas presupuestos

-- MENU REGLAS

mostrarRegla :: Int -> Regla -> String
mostrarRegla i r =
    show i ++ ". [" ++ presupuestoRelacionado r ++ "] "
    ++ condicion r ++ " " ++ show (valorAlerta r)
    ++ " -> " ++ mensajeAlerta r

menuReglas :: [RegistroFinanciero] -> [Regla] -> [Presupuesto] -> IO ()
menuReglas registros reglas presupuestos = do
    mostrarMenuReglas
    opcion <- getLine
    case opcion of
        "1" -> do
            let mensajes = mensajesReglasActivadas reglas registros
            if null mensajes
                then putStrLn "No hay reglas activadas."
                else mapM_ putStrLn mensajes
            menuReglas registros reglas presupuestos
        "2" -> do
            if null reglas
                then putStrLn "No hay reglas registradas."
                else mapM_ putStrLn (resumenTodasLasReglas reglas registros)
            menuReglas registros reglas presupuestos
        "3" -> do
            putStrLn "Categoria relacionada:"
            cat <- getLine
            putStrLn "Condicion (>, <, >=, <=, ==):"
            cond <- getLine
            putStrLn "Valor de alerta:"
            val <- readLn :: IO Double
            putStrLn "Mensaje de alerta:"
            msg <- getLine
            let nueva = Regla { condicion = cond, valorAlerta = val, presupuestoRelacionado = cat, mensajeAlerta = msg }
            guardarReglas (reglas ++ [nueva])
            putStrLn "Regla agregada."
            cicloPrincipal
        "4" -> do
            if null reglas
                then putStrLn "No hay reglas registradas."
                else do
                    mapM_ putStrLn (zipWith mostrarRegla [1..] reglas)
                    putStrLn "Numero de regla a eliminar:"
                    idx <- readLn :: IO Int
                    if idx < 1 || idx > length reglas
                        then putStrLn "Indice invalido."
                        else do
                            guardarReglas (take (idx - 1) reglas ++ drop idx reglas)
                            putStrLn "Regla eliminada."
            cicloPrincipal
        "5" -> cicloPrincipal
        _   -> do
            putStrLn "Opcion invalida."
            menuReglas registros reglas presupuestos

-- MENU REPORTES

menuReportes :: [RegistroFinanciero] -> [Regla] -> [Presupuesto] -> IO ()
menuReportes registros reglas presupuestos = do
    mostrarMenuReportes
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ano:"
            ano <- readLn :: IO Integer
            putStrLn "Mes (1-12):"
            mes <- readLn :: IO Int
            putStrLn (resumenMensual ano mes registros)
            menuReportes registros reglas presupuestos
        "2" -> do
            putStrLn "Ano del primer periodo:"
            ano1 <- readLn :: IO Integer
            putStrLn "Mes del primer periodo:"
            mes1 <- readLn :: IO Int
            putStrLn "Ano del segundo periodo:"
            ano2 <- readLn :: IO Integer
            putStrLn "Mes del segundo periodo:"
            mes2 <- readLn :: IO Int
            putStrLn (compararPeriodos (ano1, mes1) (ano2, mes2) registros)
            menuReportes registros reglas presupuestos
        "3" -> do
            putStrLn "Cuantas categorias mostrar:"
            n <- readLn :: IO Int
            let reporte = reporteTopCategorias n registros
            if null reporte
                then putStrLn "No hay datos de gastos."
                else mapM_ putStrLn reporte
            menuReportes registros reglas presupuestos
        "4" -> cicloPrincipal
        _   -> do
            putStrLn "Opcion invalida."
            menuReportes registros reglas presupuestos
