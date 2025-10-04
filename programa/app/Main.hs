{-# LANGUAGE OverloadedStrings #-}

module Main where

import Utilidades
import Venta
import Data.List (sortOn)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)


main :: IO ()
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
