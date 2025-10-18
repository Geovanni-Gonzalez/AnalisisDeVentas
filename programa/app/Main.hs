{-# LANGUAGE OverloadedStrings #-}

module Main where

import Utilidades
import Venta
import Procesamiento
import Data.List (sortOn)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Encode.Pretty (encodePretty)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map

-- | Guarda una lista de ventas en el archivo JSON principal
guardarVentasEnArchivo :: [Venta] -> IO ()
guardarVentasEnArchivo ventas = do
    B.writeFile "src/data/Ventas.json" (encodePretty ventas)
    putStrLn "Datos guardados en src/data/Ventas.json"

menuAnalisisDeDatos :: [Venta] -> IO ()
menuAnalisisDeDatos ventas = do
    putStrLn "=== Menú de Análisis de Datos ==="
    putStrLn "1. Total de ventas(suma de importes)"
    putStrLn "2. Total de ventas mensuales y anuales"
    putStrLn "3. Promedio de ventas por categoria por año"
    putStrLn "4. Volver al menú principal"
    putStr "Seleccione una opción (1-4): "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            let total = totalDeVentas ventas
            putStrLn $ "Total de ventas: " ++ show total
            putStrLn ""
            menuAnalisisDeDatos ventas
        "2" -> do
            let totales = totalVentasMensualesAnuales ventas
            if null totales
                then putStrLn "No hay ventas registradas."
                else do
                    putStrLn "Totales de ventas por mes:"
                    mapM_ (\(mes, total) -> putStrLn $ "  " ++ mes ++ " -> " ++ show total) totales
                    putStrLn ""
            menuAnalisisDeDatos ventas
        
        "3" -> do
            let promedios = promedioVentasPorCategoriaAnual ventas
            if null promedios
                then putStrLn "No hay ventas registradas."
                else do
                    putStrLn "Promedio de ventas por categoría:"
                    mapM_ (\(cat, prom) -> putStrLn $ "  " ++ cat ++ " -> " ++ show prom) promedios
                    putStrLn ""
            menuAnalisisDeDatos ventas

        "4" -> menuPrincipal ventas
        _   -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuAnalisisDeDatos ventas

menuAnalisisTemporal :: [Venta] -> IO ()
menuAnalisisTemporal ventas = do
    putStrLn "=== Menú de Análisis Temporal ==="
    putStrLn "1. Mes con mayor venta y día de la semana más activo"
    putStrLn "2. Tasa de crecimiento/decrecimiento en un trimestre específico"
    putStrLn "3. Resumen de ventas por trimestre"
    putStrLn "4. Volver al menú principal"
    putStr "Seleccione una opción (1-4): "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            case mesMayorVenta ventas of
                Just (mes, total) -> putStrLn $ "Mes con mayor venta: " ++ mes ++ " -> " ++ show total
                Nothing -> putStrLn "No hay ventas registradas."
            case diaMasActivo ventas of
                Just (dia, count) -> putStrLn $ "Día más activo: " ++ diasSemanaES dia ++ " -> " ++ show count ++ " transacciones"
                Nothing -> putStrLn "No se pudo determinar el día más activo."
            putStrLn ""
            menuAnalisisTemporal ventas
        "2" -> do
            putStr "Ingrese el trimestre (formato AAAA-Tn, ej: 2025-T2): "
            hFlush stdout
            trimestre <- getLine
            case tasaCrecimientoTrimestral ventas trimestre of
                Just tasa -> putStrLn $ "Tasa de crecimiento: " ++ show tasa ++ "%"
                Nothing -> putStrLn "No se puede calcular la tasa (trimestre anterior sin ventas o formato inválido)."
            putStrLn ""
            menuAnalisisTemporal ventas
        "3" -> do
            let resumen = resumenTrimestral ventas
            if null resumen
                then putStrLn "No hay ventas registradas."
                else do
                    putStrLn "Resumen de ventas por trimestre:"
                    mapM_ (\(tr, total) -> putStrLn $ "  " ++ tr ++ " -> " ++ show total) resumen
            menuAnalisisTemporal ventas
        "4" -> return ()  -- Volver al menú principal
        _ -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuAnalisisTemporal ventas

menuBusquedaEspecifica :: [Venta] -> IO ()
menuBusquedaEspecifica ventas = do
    putStrLn "=== Menú de Búsqueda Específica ==="
    putStrLn "1. Buscar ventas por rango de fechas"
    putStrLn "2. Volver al menú principal"
    putStr "Seleccione una opción (1-2): "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Ingrese la fecha de inicio (YYYY-MM-DD): "
            hFlush stdout
            fechaInicio <- getLine
            putStr "Ingrese la fecha de fin (YYYY-MM-DD): "
            hFlush stdout
            fechaFin <- getLine
            let resultados = buscarVentasPorRangoDeFechas fechaInicio fechaFin ventas
            if null resultados
                then putStrLn "No se encontraron ventas en el rango especificado."
                else do
                    putStrLn $ "Se encontraron " ++ show (length resultados) ++ " ventas:"
                    mapM_ mostrarVenta resultados
            putStrLn ""
            menuBusquedaEspecifica ventas


        "2" -> return ()  -- Volver al menú principal
        _ -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuBusquedaEspecifica ventas

-- | Función auxiliar para mostrar una venta de forma legible
mostrarVenta :: Venta -> IO ()
mostrarVenta venta = do
    putStrLn $ "  ID: " ++ show (idVenta venta) ++ 
               " | Fecha: " ++ fecha venta ++ 
               " | Producto: " ++ nombreProducto venta ++
               " | Categoría: " ++ categoria venta ++
               " | Cantidad: " ++ show (cantidad venta) ++
               " | Precio: $" ++ show (precioUnitario venta) ++
               " | Total: $" ++ show (totalVenta venta)

menuPrincipal :: [Venta] -> IO ()
menuPrincipal ventas = do
    putStrLn "=== Menú Principal ==="
    putStrLn "1. Importación de datos"
    putStrLn "2. Procesamiento de datos"
    putStrLn "3. Análisis de datos"
    putStrLn "4. Análisis temporal"
    putStrLn "5. Búsqueda específica"
    putStrLn "6. Estadísticas"
    putStrLn "7. Salir"
    putStr "Seleccione una opción (1-7):"
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Ingrese la ruta del archivo JSON a importar:"
            hFlush stdout
            ruta <- getLine
            resultado <- importarVenta ruta ventas
            case resultado of
                Left err -> putStrLn $ "Error al importar ventas: " ++ err
                Right ventasActualizadas -> do
                    putStrLn "Ventas importadas y guardadas exitosamente."
                    menuPrincipal ventasActualizadas
        "2" -> do
            ventasProcesadas <- menuProcesamiento ventas
            menuPrincipal ventasProcesadas   
        "3" -> do
            menuAnalisisDeDatos ventas
            menuPrincipal ventas
        "4" -> do
            menuAnalisisTemporal ventas
            menuPrincipal ventas
        "5" -> do
            menuBusquedaEspecifica ventas
            menuPrincipal ventas
        "6" -> do
            putStrLn "Funcionalidad de estadísticas aún no implementada (DARYLL)."
        "7" -> putStrLn "Saliendo del programa. ¡Hasta luego!"
        _   -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuPrincipal ventas

menuProcesamiento :: [Venta] -> IO [Venta]
menuProcesamiento ventas = do
    putStrLn "=== Menú de Procesamiento de Datos ==="
    putStrLn "1. Completar cantidad faltante"
    putStrLn "2. Completar precio unitario faltante"
    putStrLn "3. Eliminar duplicados"
    putStrLn "4. Volver al menú principal"
    putStr "Seleccione una opción (1-4): "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Seleccione la técnica estadística:"
            putStrLn "1. Media"
            putStrLn "2. Mediana"
            putStrLn "3. Moda"
            putStr "Técnica (1-3): "
            hFlush stdout
            tecnicaStr <- getLine
            let tecnica = case tecnicaStr of
                    "1" -> Media
                    "2" -> Mediana
                    "3" -> Moda
                    _   -> Media
            let resultado = completarCantidadFaltante ventas tecnica
            putStrLn $ "Procesamiento completado:"
            putStrLn $ "- Técnica utilizada: " ++ tecnicaUtilizada resultado
            putStrLn $ "- Registros modificados: " ++ show (length $ registrosModificados resultado)
            putStrLn $ "- IDs modificados: " ++ show (registrosModificados resultado)
            putStrLn ""
            -- Guardar los datos procesados
            guardarVentasEnArchivo (ventasProcesadas resultado)
            return (ventasProcesadas resultado)
        
        "2" -> do
            putStrLn "Seleccione la técnica estadística:"
            putStrLn "1. Media"
            putStrLn "2. Mediana"
            putStrLn "3. Moda"
            putStr "Técnica (1-3): "
            hFlush stdout
            tecnicaStr <- getLine
            let tecnica = case tecnicaStr of
                    "1" -> Media
                    "2" -> Mediana
                    "3" -> Moda
                    _   -> Media
            let resultado = completarPrecioUnitarioFaltante ventas tecnica
            putStrLn $ "Procesamiento completado:"
            putStrLn $ "- Técnica utilizada: " ++ tecnicaUtilizada resultado
            putStrLn $ "- Registros modificados: " ++ show (length $ registrosModificados resultado)
            putStrLn $ "- IDs modificados: " ++ show (registrosModificados resultado)
            putStrLn ""
            -- Guardar los datos procesados
            guardarVentasEnArchivo (ventasProcesadas resultado)
            return (ventasProcesadas resultado)
            
        "3" -> do
            let resultado = eliminarDuplicados ventas
            putStrLn $ "Procesamiento completado:"
            putStrLn $ "- Técnica utilizada: " ++ tecnicaUtilizada resultado
            putStrLn $ "- Registros eliminados: " ++ show (length $ registrosModificados resultado)
            putStrLn $ "- IDs eliminados: " ++ show (registrosModificados resultado)
            putStrLn ""
            -- Guardar los datos procesados
            guardarVentasEnArchivo (ventasProcesadas resultado)
            return (ventasProcesadas resultado)
            
        "4" -> return ventas
        _   -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuProcesamiento ventas




main :: IO ()
-- Main para probar las funciones implementadas (Análisis temporal y carga de datos)
main = do
    ventasActuales <- cargarVentasDeArchivo "src/data/Ventas.json"
    case ventasActuales of
        Left err -> putStrLn $ "Error al cargar ventas: " ++ err
        Right ventas -> do
            putStrLn "Ventas cargadas exitosamente."
            menuPrincipal ventas