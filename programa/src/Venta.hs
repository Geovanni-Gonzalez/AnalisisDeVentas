{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Módulo      : Venta
-- Descripción : Gestión y análisis de ventas
-- Licencia    : MIT
-- Autor       : Geovanni Gonzalez, Gerny Diaz, Daryll Martinez
-- Fecha       : 2025-10-04
--
-- Este módulo define la estructura de datos 'Venta' y proporciona funciones
-- para análisis de ventas, incluyendo cálculo de ventas por mes, trimestre,
-- crecimiento trimestral y días con mayor actividad. Las funciones están
-- pensadas para usarse en sistemas de análisis financiero o de gestión de
-- inventario, utilizando listas de ventas como entrada.
module Venta where  

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (Day, parseTimeM, defaultTimeLocale, formatTime)
import Data.Maybe (fromMaybe)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
-- -----------------------------------------------------------------------------
-- | Representa una venta realizada en un sistema de gestión.
-- 
-- Cada venta contiene un identificador único, fecha, información del producto,
-- cantidad vendida, precio unitario y total de la venta.
-- 
-- Campos:
-- 
-- * 'idVenta'       : Identificador único de la venta.
-- * 'fecha'         : Fecha de la venta, en formato "YYYY-MM-DD".
-- * 'idProducto'    : Identificador del producto vendido.
-- * 'nombreProducto': Nombre del producto.
-- * 'categoria'     : Categoría del producto.
-- * 'cantidad'      : Cantidad de unidades vendidas.
-- * 'precioUnitario': Precio unitario del producto.
-- * 'totalVenta'    : Total de la venta (cantidad * precioUnitario).
data Venta = Venta
  { idVenta       :: Int
  , fecha         :: String  -- formato "YYYY-MM-DD"
  , idProducto    :: String
  , nombreProducto:: String
  , categoria     :: String
  , cantidad      :: Int
  , precioUnitario:: Int
  , totalVenta    :: Int
  } deriving (Show, Generic)

-- Instancias para conversión a JSON
instance ToJSON Venta
instance FromJSON Venta

-- -----------------------------------------------------------------------------
-- | Determina el mes con mayor monto de ventas.
--  Autor: Geovanni Gonzalez
-- Esta función agrupa las ventas por mes (formato "YYYY-MM") y suma el total
-- de cada mes, devolviendo el mes con mayor suma.
--
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- * 'Nothing' si la lista está vacía.
-- * 'Just (mes, total)' con el mes y la suma total de ventas.
--
-- ==== Ejemplo
--
-- >>> mesMayorVenta [Venta 1 "2025-01-15" "P1" "Producto1" "Cat1" 2 100 200,
--                    Venta 2 "2025-01-20" "P2" "Producto2" "Cat2" 1 150 150,
--                    Venta 3 "2025-02-10" "P3" "Producto3" "Cat1" 3 100 300]
-- Just ("2025-02",300)
mesMayorVenta :: [Venta] -> Maybe (String, Int)
mesMayorVenta ventas =
  let ventasPorMes = foldl' agruparPorMes [] ventas
  in if null ventasPorMes
     then Nothing
     else Just $ foldl1 (\(m1,v1) (m2,v2) -> if v1 > v2 then (m1,v1) else (m2,v2)) ventasPorMes
  where
    agruparPorMes acc venta =
      let mes = take 7 (fecha venta)  -- "YYYY-MM"
          monto = totalVenta venta
      in case lookup mes acc of
          Just total -> (mes, total + monto) : filter ((/= mes) . fst) acc
          Nothing    -> (mes, monto) : acc

-- -----------------------------------------------------------------------------
-- | Calcula la tasa de crecimiento o decrecimiento de ventas entre trimestres.
--  Autor: Geovanni Gonzalez
-- El cálculo se basa en comparar el total del trimestre indicado con el
-- trimestre anterior.
--
-- ==== Parámetros
--
-- * 'ventas'    : Lista de ventas a analizar.
-- * 'trimestre' : Trimestre a analizar en formato "YYYY-Tx".
--
-- ==== Retorno
--
-- * 'Nothing' si el trimestre anterior tiene total de ventas cero.
-- * 'Just porcentaje' con el porcentaje de crecimiento/decrecimiento.
--
-- ==== Ejemplo
--
-- >>> tasaCrecimientoTrimestral [Venta 1 "2025-04-01" "P1" "Prod" "Cat" 5 100 500] "2025-T2"
-- Nothing
tasaCrecimientoTrimestral :: [Venta] -> String -> Maybe Double
tasaCrecimientoTrimestral ventas trimestre =
  let ventasActual  = filter (\v -> obtenerTrimestre (fecha v) == trimestre) ventas
      totalActual   = sum $ map totalVenta ventasActual
      trimestreAnt  = anterior trimestre
      ventasPrev    = filter (\v -> obtenerTrimestre (fecha v) == trimestreAnt) ventas
      totalPrev     = sum $ map totalVenta ventasPrev
  in if totalPrev == 0
     then Nothing
     else Just $ (fromIntegral (totalActual - totalPrev) / fromIntegral totalPrev) * 100

-- -----------------------------------------------------------------------------
-- | Genera un resumen de ventas agrupado por trimestre.
--  Autor: Geovanni Gonzalez
-- Cada trimestre se identifica como "YYYY-Tx".
--
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- Lista de tuplas ('Trimestre', 'TotalVentas').
--
-- ==== Ejemplo
--
-- >>> resumenTrimestral [Venta 1 "2025-01-10" "P1" "Prod" "Cat" 1 100 100]
-- [("2025-T1",100)]
resumenTrimestral :: [Venta] -> [(String, Int)]
resumenTrimestral ventas = foldl' agruparPorTrimestre [] ventas
  where
    agruparPorTrimestre acc venta =
      let trimestre = obtenerTrimestre (fecha venta)
          monto     = totalVenta venta
      in case lookup trimestre acc of
          Just total -> (trimestre, total + monto) : filter ((/= trimestre) . fst) acc
          Nothing    -> (trimestre, monto) : acc

-- -----------------------------------------------------------------------------
-- | Obtiene el trimestre de una fecha.
--  Autor: Geovanni Gonzalez
-- ==== Parámetros
--
-- * 'fechaStr' : Fecha en formato "YYYY-MM-DD".
--
-- ==== Retorno
--
-- Trimestre en formato "YYYY-Tx".
--
-- ==== Ejemplo
--
-- >>> obtenerTrimestre "2025-04-15"
-- "2025-T2"
obtenerTrimestre :: String -> String
obtenerTrimestre fechaStr =
  let year = take 4 fechaStr
      mes  = read (take 2 (drop 5 fechaStr)) :: Int
      trimestre
        | mes <= 3  = "T1"
        | mes <= 6  = "T2"
        | mes <= 9  = "T3"
        | otherwise = "T4"
  in year ++ "-" ++ trimestre

-- -----------------------------------------------------------------------------
-- | Devuelve el trimestre anterior dado un trimestre.
--  Autor: Geovanni Gonzalez
-- ==== Parámetros
--
-- * 'str' : Trimestre actual en formato "YYYY-Tx".
--
-- ==== Retorno
--
-- Trimestre anterior en formato "YYYY-Tx".
--
-- ==== Ejemplo
--
-- >>> anterior "2025-T1"
-- "2024-T4"
anterior :: String -> String
anterior str =
  case span (/='-') str of
    (anioStr, '-':'T':numStr) ->
      let anio = read anioStr :: Int
          trimestre = read numStr :: Int
      in if trimestre == 1
         then show (anio - 1) ++ "-T4"
         else anioStr ++ "-T" ++ show (trimestre - 1)
    _ -> str -- Si el formato no es válido, devuelve el mismo string

-- -----------------------------------------------------------------------------
-- | Determina el día de la semana con más ventas.
--  Autor: Geovanni Gonzalez
-- Esta función cuenta la cantidad de ventas por día de la semana y devuelve
-- el día con mayor actividad.
--  
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a analizar.
--
-- ==== Retorno
--
-- * 'Nothing' si la lista de ventas está vacía.
-- * 'Just (dia, cantidad)' con el día de la semana y el número de ventas.
--
-- ==== Ejemplo
--
-- >>> diaMasActivo [Venta 1 "2025-01-15" "P1" "Prod" "Cat" 1 100 100]
-- Just ("Wednesday",1)
diaMasActivo :: [Venta] -> Maybe (String, Int)
diaMasActivo ventas =
  let dias = foldl' agruparDias [] ventas
  in if null dias
     then Nothing
     else Just $ foldl1 (\(d1,c1) (d2,c2) -> if c1 > c2 then (d1,c1) else (d2,c2)) dias
  where
    agruparDias acc venta =
      case diaSemana (fecha venta) of
        Just dia -> case lookup dia acc of
                      Just total -> (dia, total + 1) : filter ((/= dia) . fst) acc
                      Nothing    -> (dia, 1) : acc
        Nothing  -> acc

-- -----------------------------------------------------------------------------
-- | Convierte una fecha "YYYY-MM-DD" al día de la semana.
--  Autor: Geovanni Gonzalez
-- ==== Parámetros
--
-- * 'fechaStr' : Fecha a convertir.
--
-- ==== Retorno
--
-- * 'Nothing' si la fecha no se puede parsear.
-- * 'Just dia' con el nombre del día de la semana ("Monday", "Tuesday", etc.).
--
-- ==== Ejemplo
--
-- >>> diaSemana "2025-10-04"
-- Just "Saturday"
diaSemana :: String -> Maybe String
diaSemana fechaStr =
  formatTime defaultTimeLocale "%A" <$> (parseTimeM True defaultTimeLocale "%Y-%m-%d" fechaStr :: Maybe Day)



-- -----------------------------------------------------------------------------
-- | Calcula el total de ventas en una lista de ventas.
--  Autor: Geovanni Gonzalez
-- ==== Parámetros
-- * 'ventas' : Lista de ventas a sumar.
-- ==== Retorno
-- Total de ventas como un entero.
-- ==== Ejemplo
-- >>> totalDeVentas [Venta 1 "2025-01-10" "P1" "Prod" "Cat" 1 100 100,
--                    Venta 2 "2025-01-15" "P2" "Prod2" "Cat2" 2 150 300]
-- 400
totalDeVentas :: [Venta] -> Int
totalDeVentas ventas = sum $ map totalVenta ventas

-- -----------------------------------------------------------------------------
-- | Muestra el total de ventas mensuales y anuales.
--  Autor: Geovanni Gonzalez
-- ==== Parámetros
-- * 'ventas' : Lista de ventas a analizar.
-- ==== Retorno
-- Lista de tuplas con (Mes "YYYY-MM", TotalVentas).
-- ==== Ejemplo
-- >>> totalVentasMensualesAnuales [Venta 1 "2025-01-10" "P1" "Prod" "Cat" 1 100 100,
--                                    Venta 2 "2025-01-15" "P2" "Prod2" "Cat2" 2 150 300]
-- [("2025-01",400)]
totalVentasMensualesAnuales :: [Venta] -> [(String, Int)]
totalVentasMensualesAnuales ventas =
  map (\(m, vs) -> (m, sum $ map totalVenta vs)) (Map.toList $ agruparPorMes ventas)
  where
    agruparPorMes :: [Venta] -> Map.Map String [Venta]
    agruparPorMes vs =
      foldr
        (\v acc ->
            let mes = take 7 (fecha v)  -- "YYYY-MM"
            in Map.insertWith (++) mes [v] acc)
        Map.empty
        vs

-- -----------------------------------------------------------------------------
-- | Muestra el promedio de ventas por categoría por año.
--  Autor: Geovanni Gonzalez
-- ==== Parámetros
-- * 'ventas' : Lista de ventas a analizar.
-- ==== Retorno
-- Lista de tuplas con (Categoría, PromedioVentas).
-- ==== Ejemplo
-- >>> promedioVentasPorCategoriaAnual [Venta 1 "2025-01-10" "P1" "Prod" "Cat1" 1 100 100,
--                                        Venta 2 "2025-01-15" "P2" "Prod2" "Cat1" 2 150 300,
--                                        Venta 3 "2025-02-20" "P3" "Prod3" "Cat2" 1 200 200]
-- [("Cat1",200.0),("Cat2",200.0)]
promedioVentasPorCategoriaAnual :: [Venta] -> [(String, Double)]
promedioVentasPorCategoriaAnual ventas =
  map calcularPromedio (Map.toList $ agruparPorCategoria ventas)
  where
    agruparPorCategoria :: [Venta] -> Map.Map String [Int]
    agruparPorCategoria vs =
      foldr
        (\v acc -> Map.insertWith (++) (categoria v) [totalVenta v] acc)
        Map.empty
        vs
    calcularPromedio (cat, montos) =
      let total = sum montos
          count = fromIntegral (length montos)
      in (cat, fromIntegral total / count)
-- -----------------------------------------------------------------------------
-- | Busca ventas dentro de un rango de fechas específico.
--  Autor: Gerny Diaz
--  Filtra la lista de ventas para devolver solo aquellas cuya fecha se
--  encuentra entre la fecha de inicio y la fecha de fin (inclusivas).
--  Los resultados se ordenan cronológicamente por fecha.
--
-- ==== Parámetros
--
-- * 'fechaInicio' : Fecha inicial del rango en formato "YYYY-MM-DD".
-- * 'fechaFin'    : Fecha final del rango en formato "YYYY-MM-DD".
-- * 'ventas'      : Lista de ventas a filtrar.
--
-- ==== Retorno
--
-- * Lista de 'Venta' que cumplen con el criterio del rango de fechas, ordenadas por fecha.
--
-- ==== Ejemplo
--
-- >>> buscarVentasPorRangoDeFechas "2025-10-01" "2025-10-02" [Venta 1 "2025-10-01" "P1" "Prod" "Cat" 1 100 100, Venta 3 "2025-10-03" "P3" "Prod" "Cat" 1 100 100]
-- [Venta {idVenta = 1, fecha = "2025-10-01", ...}]
buscarVentasPorRangoDeFechas :: String -> String -> [Venta] -> [Venta]
buscarVentasPorRangoDeFechas fechaInicio fechaFin ventas =
  ordenarVentasPorFecha $ filter (\venta -> let f = fecha venta in f >= fechaInicio && f <= fechaFin) ventas


-- | Ordena una lista de ventas cronológicamente por fecha.
ordenarVentasPorFecha :: [Venta] -> [Venta]
ordenarVentasPorFecha = sortBy (comparing fecha)