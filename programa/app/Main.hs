{-# LANGUAGE OverloadedStrings #-}

module Main where

import Utilidades

main :: IO ()






-- Main para probar las funciones de cargar e importar ventas
--main = do
--    ventasActuales <- cargarVentasDeArchivo "src/data/Ventas.json"
--   case ventasActuales of
--        Left err ->
--            putStrLn $ "Error al cargar ventas: " ++ err
--        Right ventas -> do
--            putStrLn "Ventas cargadas:"
--            mapM_ print ventas
--            resultadoImportacion <- importarVenta "src/lote.json" ventas
--            case resultadoImportacion of
--                Left err -> putStrLn $ "Error al importar ventas: " ++ err
--                Right ventasActualizadas -> do
--                    putStrLn "Ventas actualizadas:"
--                    mapM_ print ventasActualizadas
