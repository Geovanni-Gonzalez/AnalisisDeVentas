{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Estadisticas where

import Venta
import Data.List (nub, minimumBy, maximumBy)
import Data.Ord (comparing)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- ============================================================
-- C. CATEGORÍA CON MENOR PARTICIPACIÓN (por cantidad)
-- ============================================================

categoriaConMenorParticipacion :: [Venta] -> Maybe (String, Int)
categoriaConMenorParticipacion ventas
    | null conteos = Nothing
    | otherwise    = Just $ minimumBy (comparing snd) conteos
  where
    conteos = Map.toList $ foldr acumular Map.empty ventas
    acumular venta acc =
        let cat = categoria venta
            cant = cantidad venta
            actual = fromMaybe 0 (Map.lookup cat acc)
        in Map.insert cat (actual + cant) acc

generarReporteCategoriaMenorParticipacionJSON :: [Venta] -> B.ByteString
generarReporteCategoriaMenorParticipacionJSON ventas =
    case categoriaConMenorParticipacion ventas of
        Nothing -> encodePretty $ object ["mensaje" .= ("No hay datos" :: String)]
        Just (cat, cant) -> encodePretty $ object ["categoria" .= cat, "cantidad" .= cant]

generarReporteCategoriaMenorParticipacionCSV :: FilePath -> [Venta] -> IO ()
generarReporteCategoriaMenorParticipacionCSV ruta ventas =
    case categoriaConMenorParticipacion ventas of
        Nothing -> writeFile ruta "mensaje\nNo hay datos"
        Just (cat, cant) -> writeFile ruta $ "categoria,cantidad\n" ++ cat ++ "," ++ show cant

-- ============================================================
-- D. RESUMEN GENERAL DE VENTAS
-- ============================================================

data ResumenGeneral = ResumenGeneral
    { cantidadPorCategoria      :: [(String, Int)]
    , ventaMasAlta              :: Maybe Venta
    , ventaMasBaja              :: Maybe Venta
    , categoriaConMayorVar      :: Maybe String
    } deriving (Show)

instance ToJSON ResumenGeneral where
    toJSON resumen = object
        [ "cantidadPorCategoria"      .= cantidadPorCategoria resumen
        , "ventaMasAlta"              .= ventaMasAlta resumen
        , "ventaMasBaja"              .= ventaMasBaja resumen
        , "categoriaConMayorVariedad" .= categoriaConMayorVar resumen
        ]

categoriaConMayorVariedad :: [Venta] -> Maybe String
categoriaConMayorVariedad ventas
    | null variedadPorCategoria = Nothing
    | otherwise = Just $ fst $ maximumBy (comparing snd) variedadPorCategoria
  where
    variedadPorCategoria =
        [ (cat, length (nub productos))
        | (cat, productos) <- Map.toList $ foldr acumular Map.empty ventas
        ]
    acumular venta acc =
        let cat = categoria venta
            prod = idProducto venta
            actuales = fromMaybe [] (Map.lookup cat acc)
        in Map.insert cat (prod : actuales) acc

resumenGeneral :: [Venta] -> ResumenGeneral
resumenGeneral ventas = ResumenGeneral
    { cantidadPorCategoria      = Map.toList conteos
    , ventaMasAlta              = if null ventas then Nothing else Just (maximumBy (comparing totalVenta) ventas)
    , ventaMasBaja              = if null ventas then Nothing else Just (minimumBy (comparing totalVenta) ventas)
    , categoriaConMayorVar      = categoriaConMayorVariedad ventas
    }
  where
    conteos = foldr acumular Map.empty ventas
    acumular venta acc =
        let cat = categoria venta
            cant = cantidad venta
            actual = fromMaybe 0 (Map.lookup cat acc)
        in Map.insert cat (actual + cant) acc

generarResumenGeneralJSON :: [Venta] -> B.ByteString
generarResumenGeneralJSON = encodePretty . toJSON

-- |
-- Módulo      : Estadisticas
-- Descripción : Funciones para análisis estadístico 
-- Licencia    : MIT
-- Autor       : Geovanni Gonzalez, Gerny Diaz
-- Fecha       : 2025-10-18
--
-- Este módulo proporciona funciones para realizar análisis estadísticos
-- sobre datos de ventas, como cálculos de totales, promedios y agrupaciones.
--Estadísticas 
--Se deberán mostrar las siguientes estadísticas (indican el código o letra de esta): 
-- A. Top 5 de categorías con mayores ventas (monto). 
-- B. Producto más vendido (por cantidad). 
-- C. Categoría con menor participación (cantidad). 
-- D. Resumen general: 
-- • Cantidad de ventas por categoría. 
-- • Venta más alta y más baja. 
-- • Categoría con mayor variedad de productos vendidos. 
-- Cada reporte de estadísticas debe exportarse en formato csv o json.

module Estadisticas where

import Venta
import Data.List (sortBy, groupBy, maximumBy, take, reverse)
import Data.Ord (comparing)
import qualified Data.Map as Map

-- -----------------------------------------------------------------------------
-- A. Top 5 de categorías con mayores ventas (monto)
-- -----------------------------------------------------------------------------

-- | Calcula el top 5 de categorías con mayores ventas por monto total.
-- Agrupa las ventas por categoría, suma los montos totales de cada categoría,
-- ordena de mayor a menor y toma las primeras 5.
--
-- Parámetros:
-- * 'ventas' : Lista de ventas a analizar
--
-- Retorna:
-- * Lista de tuplas (categoría, monto total) de las 5 categorías con mayores ventas
top5CategoriasMayoresVentas :: [Venta] -> [(String, Int)]
top5CategoriasMayoresVentas ventas = 
    take 5 $ reverse $ sortBy (comparing snd) categoriasConTotales
  where
    -- Agrupa ventas por categoría y suma los totales
    categoriasConTotales = Map.toList $ Map.fromListWith (+) 
                         [(categoria venta, totalVenta venta) | venta <- ventas]

-- -----------------------------------------------------------------------------
-- B. Producto más vendido (por cantidad)
-- -----------------------------------------------------------------------------

-- | Encuentra el producto más vendido por cantidad total de unidades.
-- Agrupa las ventas por nombre de producto, suma las cantidades vendidas
-- y encuentra el producto con mayor cantidad total.
--
-- Parámetros:
-- * 'ventas' : Lista de ventas a analizar
--
-- Retorna:
-- * Maybe (nombre del producto, cantidad total) - Nothing si no hay ventas
productoMasVendido :: [Venta] -> Maybe (String, Int)
productoMasVendido [] = Nothing
productoMasVendido ventas = 
    Just $ maximumBy (comparing snd) productosConCantidades
  where
    -- Agrupa ventas por nombre de producto y suma las cantidades
    productosConCantidades = Map.toList $ Map.fromListWith (+) 
                           [(nombreProducto venta, cantidad venta) | venta <- ventas]

