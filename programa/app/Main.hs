{-# LANGUAGE OverloadedStrings #-}

module Main where

import Interfaz
import Utilidades
import Venta
import Procesamiento
import Estadisticas
import Data.List (sortOn)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import System.IO (hFlush, stdout, stdin)
import qualified Data.Map as Map
import Data.Ord (comparing)


-- =========================
-- Menú de Procesamiento
-- =========================

menuProcesamiento :: [Venta] -> IO [Venta]
menuProcesamiento ventas = do
    limpiarPantalla
    printHeader "PROCESAMIENTO DE DATOS"
    printMenuOption "1" "Completar cantidad faltante"
    printMenuOption "2" "Completar precio unitario faltante"
    printMenuOption "3" "Eliminar duplicados"
    printMenuOption "4" "Volver al menú principal"
    putStr "\nSeleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            limpiarPantalla
            printHeader "COMPLETAR CANTIDAD"
            printMenuOption "1" "Técnica: Media"
            printMenuOption "2" "Técnica: Mediana"
            printMenuOption "3" "Técnica: Moda"
            putStr "\nTécnica (1-3): "
            hFlush stdout
            tecnicaStr <- getLine
            let tecnica = case tecnicaStr of
                    "1" -> Media
                    "2" -> Mediana
                    "3" -> Moda
                    _   -> Media
            let resultado = completarCantidadFaltante ventas tecnica
            putStrLn ""
            printSuccess "Procesamiento completado:"
            putStrLn $ "  • Técnica: " ++ tecnicaUtilizada resultado
            putStrLn $ "  • Modificaciones: " ++ show (length $ registrosModificados resultado)
            guardarVentasEnArchivo (ventasProcesadas resultado)
            waitForKey
            return (ventasProcesadas resultado)
        "2" -> do
            limpiarPantalla
            printHeader "COMPLETAR PRECIO"
            printMenuOption "1" "Técnica: Media"
            printMenuOption "2" "Técnica: Mediana"
            printMenuOption "3" "Técnica: Moda"
            putStr "\nTécnica (1-3): "
            hFlush stdout
            tecnicaStr <- getLine
            let tecnica = case tecnicaStr of
                    "1" -> Media
                    "2" -> Mediana
                    "3" -> Moda
                    _   -> Media
            let resultado = completarPrecioUnitarioFaltante ventas tecnica
            putStrLn ""
            printSuccess "Procesamiento completado:"
            putStrLn $ "  • Técnica: " ++ tecnicaUtilizada resultado
            putStrLn $ "  • Modificaciones: " ++ show (length $ registrosModificados resultado)
            guardarVentasEnArchivo (ventasProcesadas resultado)
            waitForKey
            return (ventasProcesadas resultado)
        "3" -> do
            limpiarPantalla
            printHeader "ELIMINAR DUPLICADOS"
            let resultado = eliminarDuplicados ventas
            putStrLn ""
            printSuccess "Procesamiento completado:"
            putStrLn $ "  • Técnica: " ++ tecnicaUtilizada resultado
            putStrLn $ "  • Eliminados: " ++ show (length $ registrosModificados resultado)
            guardarVentasEnArchivo (ventasProcesadas resultado)
            waitForKey
            return (ventasProcesadas resultado)
        "4" -> return ventas
        _   -> do
            printError "Opción inválida."
            waitForKey
            menuProcesamiento ventas

-- =========================
-- Menús principales
-- =========================

menuAnalisisDeDatos :: [Venta] -> IO ()
menuAnalisisDeDatos ventas = do
    limpiarPantalla
    printHeader "ANÁLISIS DE DATOS"
    printMenuOption "1" "Total de ventas (suma de importes)"
    printMenuOption "2" "Total de ventas mensuales y anuales"
    printMenuOption "3" "Promedio de ventas por categoría por año"
    printMenuOption "4" "Volver al menú principal"
    putStr "\nSeleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            let total = totalDeVentas ventas
            putStrLn ""
            printSuccess $ "Total de ventas: $" ++ show total
            waitForKey
            menuAnalisisDeDatos ventas
        "2" -> do
            limpiarPantalla
            printHeader "VENTAS MENSUALES Y ANUALES"
            let totales = totalVentasMensualesAnuales ventas
            if null totales
                then printError "No hay ventas registradas."
                else mapM_ (\(mes, total) -> putStrLn $ "  • " ++ mes ++ " -> $" ++ show total) totales
            waitForKey
            menuAnalisisDeDatos ventas
        "3" -> do
            limpiarPantalla
            printHeader "PROMEDIO POR CATEGORÍA"
            let promedios = promedioVentasPorCategoriaAnual ventas
            if null promedios
                then printError "No hay ventas registradas."
                else mapM_ (\(cat, prom) -> putStrLn $ "  • " ++ cat ++ " -> $" ++ show prom) promedios
            waitForKey
            menuAnalisisDeDatos ventas
        "4" -> return ()
        _   -> do
            printError "Opción inválida."
            waitForKey
            menuAnalisisDeDatos ventas

menuAnalisisTemporal :: [Venta] -> IO ()
menuAnalisisTemporal ventas = do
    limpiarPantalla
    printHeader "ANÁLISIS TEMPORAL"
    printMenuOption "1" "Mes con mayor venta y día más activo"
    printMenuOption "2" "Tasa de crecimiento trimestral"
    printMenuOption "3" "Resumen de ventas por trimestre"
    printMenuOption "4" "Volver al menú principal"
    putStr "\nSeleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            limpiarPantalla
            printHeader "HITOS TEMPORALES"
            case mesMayorVenta ventas of
                Just (mes, total) -> putStrLn $ "  • Mes récord: " ++ mes ++ " ($" ++ show total ++ ")"
                Nothing -> printError "No hay ventas registradas."
            case diaMasActivo ventas of
                Just (dia, count) -> putStrLn $ "  • Día más activo: " ++ diasSemanaES dia ++ " (" ++ show count ++ " txs)"
                Nothing -> putStrLn "  • Día más activo: N/A"
            waitForKey
            menuAnalisisTemporal ventas
        "2" -> do
            putStr "\nIngrese el trimestre (ej: 2025-T2): "
            hFlush stdout
            trimestre <- getLine
            case tasaCrecimientoTrimestral ventas trimestre of
                Just tasa -> printSuccess $ "Tasa de crecimiento: " ++ show tasa ++ "%"
                Nothing -> printError "No se puede calcular (sin datos previos o formato inválido)."
            waitForKey
            menuAnalisisTemporal ventas
        "3" -> do
            limpiarPantalla
            printHeader "RESUMEN TRIMESTRAL"
            let resumen = resumenTrimestral ventas
            if null resumen
                then printError "No hay ventas registradas."
                else mapM_ (\(tr, total) -> putStrLn $ "  • " ++ tr ++ " -> $" ++ show total) resumen
            waitForKey
            menuAnalisisTemporal ventas
        "4" -> return ()
        _ -> do
            printError "Opción inválida."
            waitForKey
            menuAnalisisTemporal ventas

menuBusquedaEspecifica :: [Venta] -> IO ()
menuBusquedaEspecifica ventas = do
    limpiarPantalla
    printHeader "BÚSQUEDA ESPECÍFICA"
    printMenuOption "1" "Buscar ventas por rango de fechas"
    printMenuOption "2" "Volver al menú principal"
    putStr "\nSeleccione una opción: "
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
            
            limpiarPantalla
            printHeader $ "RESULTADOS: " ++ fechaInicio ++ " a " ++ fechaFin
            let resultados = buscarVentasPorRangoDeFechas fechaInicio fechaFin ventas
            if null resultados
                then printError "No se encontraron ventas en ese rango."
                else mapM_ mostrarVenta resultados
            waitForKey
            menuBusquedaEspecifica ventas
        "2" -> return ()
        _ -> do
            printError "Opción inválida."
            waitForKey
            menuBusquedaEspecifica ventas

menuEstadisticas :: [Venta] -> IO ()
menuEstadisticas ventas = do
    limpiarPantalla
    printHeader "ESTADÍSTICAS AVANZADAS"
    printMenuOption "1" "Top 5 Categorías (Monto)"
    printMenuOption "2" "Producto más vendido (Cantidad)"
    printMenuOption "3" "Categoría con menor participación"
    printMenuOption "4" "Resumen General (KPIs)"
    printMenuOption "5" "Regresar al menú principal"
    putStr "\nSeleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            let top5 = top5CategoriasMayoresVentas ventas
            limpiarPantalla
            printHeader "TOP 5 CATEGORÍAS"
            if null top5
                then printError "No hay ventas registradas."
                else do
                    mapM_ (\(c, m) -> printf "  • %-20s -> $%d\n" c m) top5
                    exportarTop5CategoriasJSONFijo top5
                    printSuccess "Reporte exportado JSON."
            waitForKey
            menuEstadisticas ventas
        "2" -> do
            case productoMasVendido ventas of
                Nothing -> printError "No hay ventas registradas."
                Just (producto, cantidad) -> do
                    limpiarPantalla
                    printHeader "PRODUCTO ESTRELLA"
                    putStrLn $ "  • Producto: " ++ producto
                    putStrLn $ "  • Unidades: " ++ show cantidad
                    exportarProductoMasVendidoJSONFijo producto cantidad
                    printSuccess "Reporte exportado JSON."
            waitForKey
            menuEstadisticas ventas
        "3" -> do
            case categoriaConMenorParticipacion ventas of
                Nothing -> printError "No hay ventas registradas."
                Just (cat, cant) -> do
                    limpiarPantalla
                    printHeader "MENOR PARTICIPACIÓN"
                    putStrLn $ "  • Categoría: " ++ cat
                    putStrLn $ "  • Unidades: " ++ show cant
                    exportarCategoriaMenorParticipacionJSONFijo cat cant
                    printSuccess "Reporte exportado JSON."
            waitForKey
            menuEstadisticas ventas
        "4" -> do
            let resumen = resumenGeneral ventas
            limpiarPantalla
            printHeader "RESUMEN GENERAL (KPIs)"
            putStrLn $ "  • Categorías: " ++ show (cantidadPorCategoria resumen)
            printMenuOption "HIGH" ("Venta Más Alta: " ++ maybe "N/A" (show . totalVenta) (ventaMasAlta resumen))
            printMenuOption "LOW" ("Venta Más Baja: " ++ maybe "N/A" (show . totalVenta) (ventaMasBaja resumen))
            putStrLn $ "  • Mayor Variedad: " ++ fromMaybe "N/A" (categoriaConMayorVar resumen)
            exportarResumenGeneralJSONFijo resumen
            printSuccess "Reporte exportado JSON."
            waitForKey
            menuEstadisticas ventas
        "5" -> return ()
        _ -> do
            printError "Opción inválida."
            waitForKey
            menuEstadisticas ventas

-- =========================
-- Menú principal
-- =========================

menuPrincipal :: [Venta] -> IO ()
menuPrincipal ventas = do
    limpiarPantalla
    printHeader "SISTEMA DE ANALISIS DE VENTAS"
    printMenuOption "1" "Importacion de datos"
    printMenuOption "2" "Procesamiento de datos (Limpieza)"
    printMenuOption "3" "Analisis de datos (Totales/Promedios)"
    printMenuOption "4" "Analisis temporal (Tendencias)"
    printMenuOption "5" "Busqueda especifica"
    printMenuOption "6" "Estadisticas Avanzadas"
    printMenuOption "7" "Salir"
    putStr "\nSeleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Ingrese ruta (JSON): "
            hFlush stdout
            ruta <- getLine
            resultado <- importarVenta ruta ventas
            case resultado of
                Left err -> do 
                    printError $ "Error: " ++ err
                    waitForKey
                    menuPrincipal ventas
                Right ventasActualizadas -> do
                    printSuccess "Ventas importadas exitosamente."
                    waitForKey
                    menuPrincipal ventasActualizadas
        "2" -> do
            datosActualizados <- cargarVentasDeArchivo "src/data/Ventas.json"
            case datosActualizados of
                Left err -> do
                    printError $ "Error al recargar: " ++ err
                    waitForKey
                    menuProcesamiento ventas >>= menuPrincipal
                Right ventasFrescas -> do
                    menuProcesamiento ventasFrescas >>= menuPrincipal
        "3" -> menuAnalisisDeDatos ventas >> menuPrincipal ventas
        "4" -> menuAnalisisTemporal ventas >> menuPrincipal ventas
        "5" -> menuBusquedaEspecifica ventas >> menuPrincipal ventas
        "6" -> menuEstadisticas ventas >> menuPrincipal ventas
        "7" -> do
            printSuccess "¡Hasta luego!"
            return ()
        _   -> do
            printError "Opción inválida."
            waitForKey
            menuPrincipal ventas

-- =========================
-- Main
-- =========================

main :: IO ()
main = do
    limpiarPantalla
    printHeader "INICIANDO SISTEMA..."
    ventasActuales <- cargarVentasDeArchivo "src/data/Ventas.json"
    case ventasActuales of
        Left err -> printError $ "Error fatal al cargar ventas: " ++ err
        Right ventas -> do
            printSuccess "Base de datos cargada correctamente."
            waitForKey
            menuPrincipal ventas
