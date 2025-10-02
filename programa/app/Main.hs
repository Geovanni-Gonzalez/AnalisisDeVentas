{-# LANGUAGE OverloadedStrings #-}

module Main where

import Utilidades

main :: IO ()
main = do
    ventasActuales <- cargarVentas "src/data/Ventas.json"
    case ventasActuales of
        Left err ->
            putStrLn $ "Error al cargar ventas: " ++ err
        Right ventas -> do
            putStrLn "Ventas cargadas:"
            mapM_ print ventas
