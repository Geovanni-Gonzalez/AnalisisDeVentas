{-# LANGUAGE OverloadedStrings #-}
-- |
-- Módulo      : Utilidades
-- Descripción : Funciones auxiliares para manejo de archivos y utilidades varias
-- Licencia    : MIT
-- Autor       : Geovanni Gonzalez, Gerny Diaz, Daryll Martinez
-- Fecha       : 2025-10-13
--
-- Este módulo proporciona funciones de apoyo para la carga, importación
-- y manipulación de datos de ventas desde archivos JSON. También incluye
-- validaciones para evitar duplicados por idVenta e idProducto al importar
-- nuevos lotes de ventas.
module Utilidades where

import System.Directory (doesFileExist)
import System.IO
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import Venta
import Data.List (nubBy)
import Data.Function (on)
import Venta

-- ----------------------------------------------------------------------------- 
-- | Carga una lista de ventas desde un archivo JSON.
--  Autor: Geovanni Gonzalez
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
-- |  Filtra ventas duplicadas por `idVenta` e `idProducto`.
-- Autor: Geovanni Gonzalez
--
-- Filtra una lista de ventas dejando solo aquellas que **no estén duplicadas**
-- ni por `idVenta` ni por `idProducto` en la lista existente.
--
-- Es decir, una venta se agrega **solo si ambos identificadores son únicos**.
--
-- ==== Parámetros
--
-- * 'listaActual'   : Lista de ventas ya registradas.
-- * 'nuevasVentas'  : Lista de ventas a importar.
--
-- ==== Retorno
--
-- * Lista de ventas que cumplen ambas condiciones de unicidad.
filtrarVentasValidas :: [Venta] -> [Venta] -> [Venta]
filtrarVentasValidas listaActual nuevasVentas =
    filter (\v -> notElem (idVenta v) (map idVenta listaActual)
               && notElem (idProducto v) (map idProducto listaActual))
           nuevasVentas

-- ----------------------------------------------------------------------------- 
-- | Importa ventas desde otro archivo JSON y las agrega a la lista existente,
-- **solo si no están duplicadas ni por idVenta ni por idProducto**.
-- Autor: Geovanni Gonzalez
-- 
-- Esta función:
--  1. Lee y decodifica el archivo JSON de origen.
--  2. Filtra ventas duplicadas (por cualquiera de los dos campos).
--  3. Agrega solo las válidas.
--  4. Guarda la lista actualizada en formato JSON legible.
--  5. Informa cuántas ventas fueron rechazadas.
--
-- ==== Parámetros
--
-- * 'ruta' : Ruta del archivo JSON a importar.
-- * 'listaVentasActual' : Lista actual de ventas.
--
-- ==== Retorno
--
-- * 'Left err' si ocurre un error de lectura o decodificación.
-- * 'Right [Venta]' con la lista actualizada y guardada.
--
-- ==== Ejemplo
--
-- >>> importarVenta "src/data/Lote.json" listaActual
-- Se omitieron 3 ventas duplicadas del lote.
-- Right [Venta 1 ..., Venta 21 ...]
importarVenta :: FilePath -> [Venta] -> IO (Either String [Venta])
importarVenta ruta listaVentasActual = do 
    contenidoByteString <- B.readFile ruta 
    let contenidoJSON = eitherDecode contenidoByteString :: Either String [Venta]
    case contenidoJSON of
        Left err -> return (Left err)
        Right ventasNuevas -> do
            let idsExistentes = map idVenta listaVentasActual
            let productosExistentes = map idProducto listaVentasActual

            -- Eliminar duplicados dentro del lote
            let ventasLoteUnicas = nubBy ((==) `on` idVenta) ventasNuevas
            let ventasLoteUnicas2 = nubBy ((==) `on` idProducto) ventasLoteUnicas

            -- Filtrar solo las válidas
            let ventasValidas = filter
                    (\v -> idVenta v `notElem` idsExistentes
                        && idProducto v `notElem` productosExistentes)
                    ventasLoteUnicas2

            -- Ventas rechazadas
            let rechazadas = [v | v <- ventasNuevas,
                                idVenta v `elem` idsExistentes
                                || idProducto v `elem` productosExistentes]

            -- Mostrar las omitidas sin emojis
            mapM_ (\v -> putStrLn $
                "Venta omitida -> idVenta: " ++ show (idVenta v)
                ++ ", idProducto: " ++ idProducto v)
                rechazadas

            -- Crear la lista final
            let listaVentasActualizadas = listaVentasActual ++ ventasValidas

            -- Guardar en formato legible
            B.writeFile "src/data/Ventas.json" (encodePretty listaVentasActualizadas)

            putStrLn $ show (length ventasValidas)
                     ++ " ventas agregadas, "
                     ++ show (length rechazadas)
                     ++ " omitidas por duplicado."

            return (Right listaVentasActualizadas)


-- ----------------------------------------------------------------------------- 
-- | Convierte el nombre de un día de la semana en inglés a español.
--  Autor: Geovanni Gonzalez
--
-- ==== Parámetros
--
-- * 'dia' : Nombre del día de la semana en inglés ("Monday", "Tuesday", etc.)
--
-- ==== Retorno
--
-- Nombre del día en español. Si no reconoce el día, devuelve el valor original.
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
