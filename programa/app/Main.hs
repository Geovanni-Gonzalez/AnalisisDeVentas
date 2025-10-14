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

menuPrincipal :: [Venta] -> IO ()
menuPrincipal ventas = do
    putStrLn "=== Menú Principal ==="
    putStrLn "1. Importación de datos"
    putStrLn "2. Procesamiento de datos"
    putStrLn "3. Análisis de datos"
    putStrLn "4. Análisis temporal"
    putStrLn "5. Estadísticas"
    putStrLn "6. Salir"
    putStr "Seleccione una opción (1-6):"
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
            putStrLn "Procesamiento de datos no implementado aún."
            menuPrincipal ventas    
        "3" -> do
            putStrLn "Análisis de datos no implementado aún."
            menuPrincipal ventas
        "4" -> do
            putStrLn "Análisis temporal no implementado aún."
            menuPrincipal ventas
        "5" -> do
            putStrLn "Estadísticas no implementado aún."
            menuPrincipal ventas
        "6" -> putStrLn "Saliendo del programa. ¡Hasta luego!"
        _   -> do
            putStrLn "Opción inválida. Intente nuevamente."
            menuPrincipal ventas




main :: IO ()
-- Main para probar las funciones implementadas (Análisis temporal y carga de datos)
main = do
    ventasActuales <- cargarVentasDeArchivo "src/data/Ventas.json"
    case ventasActuales of
        Left err -> putStrLn $ "Error al cargar ventas: " ++ err
        Right ventas -> do
            putStrLn "Ventas cargadas exitosamente."
            menuPrincipal ventas