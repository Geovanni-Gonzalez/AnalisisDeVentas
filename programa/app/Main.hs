{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import System.IO (hFlush, stdout)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

-- =========================
-- Funciones auxiliares
-- =========================

guardarVentasEnArchivo :: [Venta] -> IO ()
guardarVentasEnArchivo ventas = do
    B.writeFile "src/data/Ventas.json" (encodePretty ventas)
    putStrLn "Datos guardados en src/data/Ventas.json"

mostrarVenta :: Venta -> IO ()
mostrarVenta venta = do
    putStrLn $ "  ID: " ++ show (idVenta venta) ++ 
               " | Fecha: " ++ fecha venta ++ 
               " | Producto: " ++ nombreProducto venta ++
               " | Categoría: " ++ categoria venta ++
               " | Cantidad: " ++ show (cantidad venta) ++
               " | Precio: $" ++ show (precioUnitario venta) ++
               " | Total: $" ++ show (totalVenta venta)

-- =========================
-- Funciones de exportación
-- =========================

exportarTop5CategoriasJSONFijo :: [(String, Int)] -> IO ()
exportarTop5CategoriasJSONFijo top5 = do
    tiempo <- getCurrentTime
    let fecha = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" tiempo
    B.writeFile "src/data/top5categorias.json" $ encodePretty $
        object [ "titulo" .= ("Top 5 Categorías con Mayores Ventas" :: String)
               , "fecha_generacion" .= fecha
               , "categorias" .= map (\(c, m) -> object ["categoria" .= c, "monto" .= m]) top5
               ]
    putStrLn "Top 5 categorías exportado a /programa/data/top5categorias.json"

exportarProductoMasVendidoJSONFijo :: String -> Int -> IO ()
exportarProductoMasVendidoJSONFijo producto cantidad = do
    tiempo <- getCurrentTime
    let fecha = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" tiempo
    B.writeFile "src/data/productomasvendido.json" $ encodePretty $
        object [ "titulo" .= ("Producto Más Vendido" :: String)
               , "fecha_generacion" .= fecha
               , "producto" .= producto
               , "cantidad" .= cantidad
               ]
    putStrLn "Producto más vendido exportado a /programa/data/productomasvendido.json"

exportarCategoriaMenorParticipacionJSONFijo :: String -> Int -> IO ()
exportarCategoriaMenorParticipacionJSONFijo cat cant = do
    tiempo <- getCurrentTime
    let fecha = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" tiempo
    B.writeFile "src/data/categoriamenor.json" $ encodePretty $
        object [ "titulo" .= ("Categoría con Menor Participación" :: String)
               , "fecha_generacion" .= fecha
               , "categoria" .= cat
               , "cantidad_total" .= cant
               ]
    putStrLn "Categoría con menor participación exportada a /programa/data/categoriamenor.json"


exportarResumenGeneralJSONFijo :: ResumenGeneral -> IO ()
exportarResumenGeneralJSONFijo resumen = do
    tiempo <- getCurrentTime
    let fecha = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" tiempo
    B.writeFile "src/data/resumengeneral.json" $ encodePretty $
        object [ "titulo" .= ("Resumen General de Ventas" :: String)
               , "fecha_generacion" .= fecha
               , "cantidadPorCategoria" .= cantidadPorCategoria resumen
               , "ventaMasAlta" .= ventaMasAlta resumen
               , "ventaMasBaja" .= ventaMasBaja resumen
               , "categoriaConMayorVariedad" .= categoriaConMayorVar resumen
               ]
    putStrLn "Resumen general exportado a /programa/data/resumengeneral.json"

-- =========================
-- Menú de Procesamiento
-- =========================

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
            guardarVentasEnArchivo (ventasProcesadas resultado)
            return (ventasProcesadas resultado)
        "3" -> do
            let resultado = eliminarDuplicados ventas
            putStrLn $ "Procesamiento completado:"
            putStrLn $ "- Técnica utilizada: " ++ tecnicaUtilizada resultado
            putStrLn $ "- Registros eliminados: " ++ show (length $ registrosModificados resultado)
            putStrLn $ "- IDs eliminados: " ++ show (registrosModificados resultado)
            putStrLn ""
            guardarVentasEnArchivo (ventasProcesadas resultado)
            return (ventasProcesadas resultado)
        "4" -> return ventas
        _   -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuProcesamiento ventas

-- =========================
-- Menús principales
-- =========================

menuAnalisisDeDatos :: [Venta] -> IO ()
menuAnalisisDeDatos ventas = do
    putStrLn "=== Menú de Análisis de Datos ==="
    putStrLn "1. Total de ventas (suma de importes)"
    putStrLn "2. Total de ventas mensuales y anuales"
    putStrLn "3. Promedio de ventas por categoría por año"
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
                else mapM_ (\(mes, total) -> putStrLn $ "  " ++ mes ++ " -> " ++ show total) totales
            putStrLn ""
            menuAnalisisDeDatos ventas
        "3" -> do
            let promedios = promedioVentasPorCategoriaAnual ventas
            if null promedios
                then putStrLn "No hay ventas registradas."
                else mapM_ (\(cat, prom) -> putStrLn $ "  " ++ cat ++ " -> " ++ show prom) promedios
            putStrLn ""
            menuAnalisisDeDatos ventas
        "4" -> return ()
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
                else mapM_ (\(tr, total) -> putStrLn $ "  " ++ tr ++ " -> " ++ show total) resumen
            putStrLn ""
            menuAnalisisTemporal ventas
        "4" -> return ()
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
                else mapM_ mostrarVenta resultados
            putStrLn ""
            menuBusquedaEspecifica ventas
        "2" -> return ()
        _ -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuBusquedaEspecifica ventas

menuEstadisticas :: [Venta] -> IO ()
menuEstadisticas ventas = do
    putStrLn "=== Menú de Estadísticas ==="
    putStrLn "1. Top 5 de categorías con mayores ventas (monto)"
    putStrLn "2. Producto más vendido (por cantidad)"
    putStrLn "3. Categoría con menor participación (cantidad)"
    putStrLn "4. Resumen general de ventas"
    putStrLn "5. Regresar al menú principal"
    putStr "Seleccione una opción (1-5): "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            let top5 = top5CategoriasMayoresVentas ventas
            if null top5
                then putStrLn "No hay ventas registradas."
                else do
                    mapM_ (\(c, m) -> printf "%-20s -> %d\n" c m) top5
                    exportarTop5CategoriasJSONFijo top5
            putStrLn ""
            menuEstadisticas ventas
        "2" -> do
            case productoMasVendido ventas of
                Nothing -> putStrLn "No hay ventas registradas."
                Just (producto, cantidad) -> do
                    putStrLn $ "Producto: " ++ producto
                    putStrLn $ "Cantidad total vendida: " ++ show cantidad
                    exportarProductoMasVendidoJSONFijo producto cantidad
            putStrLn ""
            menuEstadisticas ventas
        "3" -> do
            case categoriaConMenorParticipacion ventas of
                Nothing -> putStrLn "No hay ventas registradas."
                Just (cat, cant) -> do
                    putStrLn $ "Categoría: " ++ cat
                    putStrLn $ "Cantidad total: " ++ show cant
                    exportarCategoriaMenorParticipacionJSONFijo cat cant
            putStrLn ""
            menuEstadisticas ventas
        "4" -> do
            let resumen = resumenGeneral ventas
            putStrLn $ "Cantidad por categoría: " ++ show (cantidadPorCategoria resumen)
            putStrLn $ "Venta más alta: " ++ maybe "N/A" (show . totalVenta) (ventaMasAlta resumen)
            putStrLn $ "Venta más baja: " ++ maybe "N/A" (show . totalVenta) (ventaMasBaja resumen)
            putStrLn $ "Categoría con mayor variedad: " ++ fromMaybe "N/A" (categoriaConMayorVar resumen)
            exportarResumenGeneralJSONFijo resumen
            putStrLn ""
            menuEstadisticas ventas
        "5" -> return ()
        _ -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuEstadisticas ventas

-- =========================
-- Menú principal
-- =========================

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
    putStr "Seleccione una opción (1-7): "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStr "Ingrese la ruta del archivo JSON a importar: "
            hFlush stdout
            ruta <- getLine
            resultado <- importarVenta ruta ventas
            case resultado of
                Left err -> putStrLn $ "Error al importar ventas: " ++ err
                Right ventasActualizadas -> do
                    putStrLn "Ventas importadas y guardadas exitosamente."
                    menuPrincipal ventasActualizadas
        "2" -> do
            datosActualizados <- cargarVentasDeArchivo "src/data/Ventas.json"
            case datosActualizados of
                Left err -> do
                    putStrLn $ "Error al recargar datos: " ++ err
                    menuProcesamiento ventas >>= menuPrincipal
                Right ventasFrescas -> do
                    menuProcesamiento ventasFrescas >>= menuPrincipal
        "3" -> menuAnalisisDeDatos ventas >> menuPrincipal ventas
        "4" -> menuAnalisisTemporal ventas >> menuPrincipal ventas
        "5" -> menuBusquedaEspecifica ventas >> menuPrincipal ventas
        "6" -> menuEstadisticas ventas >> menuPrincipal ventas
        "7" -> putStrLn "Saliendo del programa. ¡Hasta luego!"
        _   -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuPrincipal ventas

-- =========================
-- Main
-- =========================

main :: IO ()
main = do
    ventasActuales <- cargarVentasDeArchivo "src/data/Ventas.json"
    case ventasActuales of
        Left err -> putStrLn $ "Error al cargar ventas: " ++ err
        Right ventas -> do
            putStrLn "Ventas cargadas exitosamente."
            menuPrincipal ventas
