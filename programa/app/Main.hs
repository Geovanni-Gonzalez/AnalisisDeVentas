{-# LANGUAGE OverloadedStrings #-}

module Main where

import Utilidades
import Venta
import Data.List (sortOn)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)

main :: IO ()
-- Main para probar las funciones implementadas (Análisis temporal y carga de datos)
main = do
    ventasActuales <- cargarVentasDeArchivo "src/data/Ventas.json"
    case ventasActuales of
        Left err -> putStrLn $ "Error al cargar ventas: " ++ err
        Right ventas -> do
            putStrLn "Ventas cargadas:"
            mapM_ print ventas
            putStrLn "-----------------------------"

            -- Mes con mayor venta
            case mesMayorVenta ventas of
                Nothing -> putStrLn "No hay ventas para analizar."
                Just (mes, total) ->
                    putStrLn $ "Mes con mayor venta: " ++ mes ++ " con un total de: " ++ show total
            putStrLn "-----------------------------"

            -- Día más activo
            case diaMasActivo ventas of
                Nothing -> putStrLn "No se puede determinar el día más activo."
                Just (dia, cant) ->
                    putStrLn $ "Día de la semana más activo: " ++ diasSemanaES dia ++ " con " ++ show cant ++ " transacciones"
            putStrLn "-----------------------------"

            -- Tasa de crecimiento (ejemplo: T1 2025)
            let trimestreEjemplo = "2025-T1"
            case tasaCrecimientoTrimestral ventas trimestreEjemplo of
                Nothing -> putStrLn $ "No se puede calcular la tasa de crecimiento para " ++ trimestreEjemplo
                Just tasa -> putStrLn $ "Tasa de crecimiento para " ++ trimestreEjemplo ++ ": " ++ printf "%.2f" tasa ++ "%"
            putStrLn "-----------------------------"

            -- Resumen trimestral (ordenado por año y trimestre)
            putStrLn "Resumen trimestral de ventas:"
            let resumenOrdenado = sortOn fst (resumenTrimestral ventas)
            mapM_ (\(t, total) -> putStrLn $ t ++ ": " ++ show total) resumenOrdenado
            putStrLn "-----------------------------"

            -- Total de ventas global
            let totalVentas = sum $ map totalVenta ventas
            putStrLn $ "Total de ventas: " ++ show totalVentas
            putStrLn "-----------------------------"

            -- Totales mensuales y anuales
            putStrLn "Total de ventas mensuales y anuales:"
            let totalesMA = totalVentasMensualesAnuales ventas
            mapM_ (\(mes, total) -> putStrLn $ mes ++ ": " ++ show total) totalesMA
            putStrLn "-----------------------------"

            -- Promedio ventas por categoría anual
            let promediosCategorias = promedioVentasPorCategoriaAnual ventas
            putStrLn "Promedio de ventas por categoría anual:"
            mapM_ (\(cat, promedio) -> putStrLn $ cat ++ ": " ++ printf "%.2f" promedio) promediosCategorias
            putStrLn "-----------------------------"
