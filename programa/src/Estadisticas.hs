{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Módulo      : Estadisticas
-- Descripción : Funciones para análisis estadístico de ventas
-- Licencia    : MIT
-- Autor       : Geovanni Gonzalez, Gerny Diaz
-- Fecha       : 2025-10-18
--
-- Este módulo proporciona funciones para realizar análisis estadísticos
-- sobre datos de ventas, incluyendo cálculos de totales, promedios y agrupaciones.

module Estadisticas where

import Venta
import Data.List (nub, minimumBy, maximumBy, sortBy, reverse, take)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

-- =============================================================================
-- A. Top 5 de categorías con mayores ventas (monto)
-- =============================================================================

-- -----------------------------------------------------------------------------
-- | Calcula el top 5 de categorías con mayores ventas por monto total.
--  Autor: Gerny Diaz
--
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- Lista de tuplas (categoría, monto total) de las 5 categorías con mayores ventas.
--
-- ==== Ejemplo
--
-- >>> top5CategoriasMayoresVentas ventas
-- [("Electrónica",1500),("Ropa",1200),("Alimentos",900),("Hogar",800),("Juguetes",700)]
top5CategoriasMayoresVentas :: [Venta] -> [(String, Int)]
top5CategoriasMayoresVentas ventas = 
    take 5 $ reverse $ sortBy (comparing snd) categoriasConTotales
  where
    categoriasConTotales = Map.toList $ Map.fromListWith (+)
                         [(categoria venta, totalVenta venta) | venta <- ventas]

-- =============================================================================
-- B. Producto más vendido (por cantidad)
-- =============================================================================

-- -----------------------------------------------------------------------------
-- | Encuentra el producto más vendido por cantidad total de unidades.
--  Autor: Gerny Diaz
--
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- Maybe (nombre del producto, cantidad total). Devuelve Nothing si no hay ventas.
--
-- ==== Ejemplo
--
-- >>> productoMasVendido ventas
-- Just ("Smartphone",120)
productoMasVendido :: [Venta] -> Maybe (String, Int)
productoMasVendido [] = Nothing
productoMasVendido ventas = Just $ maximumBy (comparing snd) productosConCantidades
  where
    productosConCantidades = Map.toList $ Map.fromListWith (+)
                           [(nombreProducto venta, cantidad venta) | venta <- ventas]

-- =============================================================================
-- C. Categoría con menor participación (cantidad)
-- =============================================================================

-- -----------------------------------------------------------------------------
-- | Calcula la categoría con menor participación (por cantidad vendida).
--  Autor: Geovanni Gonzalez
--
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- Maybe (categoría, cantidad total). Devuelve Nothing si no hay ventas.
--
-- ==== Ejemplo
--
-- >>> categoriaConMenorParticipacion ventas
-- Just ("Juguetes",50)
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

-- -----------------------------------------------------------------------------
-- | Genera el reporte en formato JSON para la categoría con menor participación.
--  Autor: Geovanni Gonzalez
--
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- Objeto JSON en formato ByteString.
--
-- ==== Ejemplo
--
-- >>> generarReporteCategoriaMenorParticipacionJSON ventas
-- "{\"categoria\":\"Juguetes\",\"cantidad\":50}"
generarReporteCategoriaMenorParticipacionJSON :: [Venta] -> B.ByteString
generarReporteCategoriaMenorParticipacionJSON ventas =
    case categoriaConMenorParticipacion ventas of
        Nothing -> encodePretty $ object ["mensaje" .= ("No hay datos" :: String)]
        Just (cat, cant) -> encodePretty $ object ["categoria" .= cat, "cantidad" .= cant]

-- -----------------------------------------------------------------------------
-- | Genera el reporte en formato CSV para la categoría con menor participación.
--  Autor: Geovanni Gonzalez
--
-- ==== Parámetros
--
-- * 'ruta' : Ruta del archivo CSV a generar.
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- Acción IO que escribe el archivo CSV.
--
-- ==== Ejemplo
--
-- >>> generarReporteCategoriaMenorParticipacionCSV "reporte.csv" ventas
generarReporteCategoriaMenorParticipacionCSV :: FilePath -> [Venta] -> IO ()
generarReporteCategoriaMenorParticipacionCSV ruta ventas =
    case categoriaConMenorParticipacion ventas of
        Nothing -> writeFile ruta "mensaje\nNo hay datos"
        Just (cat, cant) -> writeFile ruta $ "categoria,cantidad\n" ++ cat ++ "," ++ show cant

-- =============================================================================
-- D. Resumen general de ventas
-- =============================================================================

-- -----------------------------------------------------------------------------
-- | Estructura que contiene un resumen general de las ventas.
--  Autor: Geovanni Gonzalez
--
-- ==== Campos
--
-- * 'cantidadPorCategoria' : Lista de tuplas (categoría, cantidad total)
-- * 'ventaMasAlta' : Maybe Venta con mayor total
-- * 'ventaMasBaja' : Maybe Venta con menor total
-- * 'categoriaConMayorVar' : Maybe String indicando la categoría con mayor variedad
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

-- -----------------------------------------------------------------------------
-- | Calcula la categoría con mayor variedad de productos vendidos.
--  Autor: Geovanni Gonzalez
--
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- Maybe String indicando la categoría con mayor variedad. Nothing si no hay ventas.
--
-- ==== Ejemplo
--
-- >>> categoriaConMayorVariedad ventas
-- Just "Electrónica"
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

-- -----------------------------------------------------------------------------
-- | Genera un resumen general de ventas.
--  Autor: Geovanni Gonzalez
--
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- ResumenGeneral con cantidad por categoría, venta más alta y baja, y categoría con mayor variedad.
--
-- ==== Ejemplo
--
-- >>> resumenGeneral ventas
-- ResumenGeneral {cantidadPorCategoria=[("Electrónica",120),("Ropa",100)], ventaMasAlta=Just ..., ventaMasBaja=Just ..., categoriaConMayorVar=Just "Electrónica"}
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

-- -----------------------------------------------------------------------------
-- | Genera el resumen general en formato JSON.
--  Autor: Geovanni Gonzalez
--
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- Objeto JSON en formato ByteString.
--
-- ==== Ejemplo
--
-- >>> generarResumenGeneralJSON ventas
-- "{\"cantidadPorCategoria\":[...],\"ventaMasAlta\":...,\"ventaMasBaja\":...,\"categoriaConMayorVariedad\":\"Electrónica\"}"
generarResumenGeneralJSON :: [Venta] -> B.ByteString
generarResumenGeneralJSON = encodePretty . toJSON
