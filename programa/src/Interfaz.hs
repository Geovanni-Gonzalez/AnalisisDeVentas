module Interfaz where

import System.Console.ANSI
import System.IO (hFlush, stdout, getChar)
import Control.Monad (when)

-- | Limpia la pantalla y coloca el cursor en la esquina superior izquierda.
limpiarPantalla :: IO ()
limpiarPantalla = clearScreen >> setCursorPosition 0 0

-- | Imprime un encabezado estilizado con borde y color (ASCII Seguro).
printHeader :: String -> IO ()
printHeader titulo = do
    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn "+--------------------------------------------------+"
    putStrLn $ "| " ++ centerText 48 titulo ++ " |"
    putStrLn "+--------------------------------------------------+"
    setSGR [Reset]
    putStrLn ""

-- | Imprime una opción de menú con estilo.
printMenuOption :: String -> String -> IO ()
printMenuOption key desc = do
    setSGR [SetColor Foreground Vivid Yellow]
    putStr $ " [" ++ key ++ "] "
    setSGR [Reset]
    putStrLn desc

-- | Imprime un mensaje de éxito en verde (ASCII Seguro).
printSuccess :: String -> IO ()
printSuccess msg = do
    setSGR [SetColor Foreground Vivid Green]
    putStrLn $ "[OK] " ++ msg
    setSGR [Reset]

-- | Imprime un mensaje de error en rojo (ASCII Seguro).
printError :: String -> IO ()
printError msg = do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "[ERROR] " ++ msg
    setSGR [Reset]

-- | Espera a que el usuario presione una tecla para continuar.
waitForKey :: IO ()
waitForKey = do
    putStrLn ""
    setSGR [SetColor Foreground Dull White]
    putStr "Presione cualquier tecla para continuar..."
    setSGR [Reset]
    hFlush stdout
    _ <- getChar
    return ()

-- | Función auxiliar para centrar texto.
centerText :: Int -> String -> String
centerText width s
    | length s >= width = take width s
    | otherwise = let pad = (width - length s) `div` 2
                  in replicate pad ' ' ++ s ++ replicate (width - length s - pad) ' '
