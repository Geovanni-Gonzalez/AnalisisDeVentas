{-# LANGUAGE OverloadedStrings #-}
-- |
-- Módulo      : Utilidades
-- Descripción : Funciones auxiliares para manejo de archivos y utilidades varias
-- Licencia    : MIT
-- Autor       : Geovanni Gonzalez, Gerny Diaz, Daryll Martinez
-- Fecha       : 2025-10-04
--
-- Este módulo proporciona funciones de apoyo para la carga, importación
-- y manipulación de datos de ventas desde archivos JSON. También incluye
-- utilidades de conversión de nombres de días de la semana al español.
module Utilidades where

import System.Directory (doesFileExist)
import System.IO
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import Venta

-- -----------------------------------------------------------------------------
-- | Carga una lista de ventas desde un archivo JSON.
--
-- La función intenta decodificar el contenido del archivo como una lista de
-- 'Venta'. Retorna un 'Either' con un mensaje de error o la lista de ventas.
--
-- ==== Parámetros
--
-- * 'ruta' : Ruta del archivo JSON a cargar.
--
-- ==== Retorno
--
-- * 'Left err' si ocurre un error de lectura o decodificación.
-- * 'Right [Venta]' con la lista de ventas cargadas correctamente.
--
-- ==== Ejemplo
--
-- >>> cargarVentasDeArchivo "src/data/Ventas.json"
-- Right [Venta 1 "2025-10-01" "P001" "Laptop" "Electrónica" 2 800 1600, ...]
cargarVentasDeArchivo :: FilePath -> IO (Either String [Venta])
cargarVentasDeArchivo ruta = do
    contenido <- B.readFile ruta
    let resultado = eitherDecode contenido :: Either String [Venta]
    return resultado

-- -----------------------------------------------------------------------------
-- | Importa ventas desde otro archivo JSON y las agrega a la lista existente.
--
-- Esta función lee un archivo JSON, decodifica la lista de ventas y las
-- agrega a la lista existente. Luego, sobrescribe el archivo principal
-- "src/data/Ventas.json" con la lista actualizada.
--
-- ==== Parámetros
--
-- * 'ruta'               : Ruta del archivo JSON a importar.
-- * 'listaVentasActual'  : Lista actual de ventas.
--
-- ==== Retorno
--
-- * 'Left err' si ocurre un error de lectura o decodificación.
-- * 'Right [Venta]' con la lista de ventas actualizada y guardada.
--
-- ==== Ejemplo
--
-- >>> importarVenta "src/data/NuevasVentas.json" listaActual
-- Right [Venta 1 ..., Venta 21 ...]
importarVenta :: FilePath -> [Venta] -> IO (Either String [Venta])
importarVenta ruta listaVentasActual = do 
    contenidoByteString <- B.readFile ruta 
    let contenidoJSON = eitherDecode contenidoByteString :: Either String [Venta]
    case contenidoJSON of
        Left err -> return (Left err)
        Right ventasNuevas -> do
            let listaVentasActualizadas = listaVentasActual ++ ventasNuevas
            B.writeFile "src/data/Ventas.json" (encode listaVentasActualizadas)
            return (Right listaVentasActualizadas)

-- -----------------------------------------------------------------------------
-- | Convierte el nombre de un día de la semana en inglés a español.
--
-- Esta función se utiliza para mostrar resultados más amigables para el
-- usuario hispanohablante.
--
-- ==== Parámetros
--
-- * 'dia' : Nombre del día de la semana en inglés ("Monday", "Tuesday", etc.)
--
-- ==== Retorno
--
-- * Nombre del día en español. Si no reconoce el día, devuelve el valor original.
--
-- ==== Ejemplo
--
-- >>> diasSemanaES "Wednesday"
-- "Miércoles"
diasSemanaES :: String -> String
diasSemanaES dia = case dia of
    "Monday"    -> "Lunes"
    "Tuesday"   -> "Martes"
    "Wednesday" -> "Miércoles"
    "Thursday"  -> "Jueves"
    "Friday"    -> "Viernes"
    "Saturday"  -> "Sábado"
    "Sunday"    -> "Domingo"
    _           -> dia
