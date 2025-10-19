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
