{-# LANGUAGE OverloadedStrings #-}
-- |
-- Módulo      : Procesamiento
-- Descripción : Funciones para procesamiento y limpieza de datos de ventas
-- Licencia    : MIT
-- Autor       : Geovanni Gonzalez, Gerny Diaz, Daryll Martinez
-- Fecha       : 2025-10-10
--
-- Este módulo proporciona funciones para procesar y limpiar datos de ventas,
-- incluyendo completar datos faltantes usando técnicas estadísticas (moda, 
-- media, mediana) y eliminar duplicados. Utiliza funciones de orden superior
-- para el procesamiento eficiente de los datos.
module Procesamiento where

import Venta
import Data.List (sort, sortBy, nub, group, groupBy)
import Data.Function (on)
import qualified Data.Map as Map

-- -----------------------------------------------------------------------------
-- | Técnicas estadísticas disponibles para completar datos faltantes
data TecnicaEstadistica = Media | Mediana | Moda
    deriving (Show, Eq)

-- -----------------------------------------------------------------------------
-- | Resultado del procesamiento de datos que incluye información sobre 
-- los registros modificados
data ResultadoProcesamiento = ResultadoProcesamiento
    { ventasProcesadas :: [Venta]
    , registrosModificados :: [Int]  -- IDs de ventas modificadas
    , tecnicaUtilizada :: String
    } deriving (Show)

-- -----------------------------------------------------------------------------
-- | Calcula la media (promedio) de una lista de números enteros.
--  Utiliza funciones de orden superior (map, foldr).
-- 
-- ==== Parámetros
--
-- * 'valores' : Lista de valores enteros.
--
-- ==== Retorno
--
-- * Media como número entero (redondeado).
--
-- ==== Ejemplo
--
-- >>> calcularMedia [10, 20, 30]
-- 20
calcularMedia :: [Int] -> Int
calcularMedia [] = 0
calcularMedia valores = 
    let suma = foldr (+) 0 valores
        cantidad = length valores
    in round (fromIntegral suma / fromIntegral cantidad)

-- -----------------------------------------------------------------------------
-- | Calcula la mediana de una lista de números enteros.
--  Utiliza funciones de orden superior (sort, map).
-- 
-- ==== Parámetros
--
-- * 'valores' : Lista de valores enteros.
--
-- ==== Retorno
--
-- * Mediana como número entero.
--
-- ==== Ejemplo
--
-- >>> calcularMediana [10, 30, 20]
-- 20
calcularMediana :: [Int] -> Int
calcularMediana [] = 0
calcularMediana valores =
    let valoresOrdenados = sort valores
        longitud = length valoresOrdenados
        mitad = longitud `div` 2
    in if longitud `mod` 2 == 0
       then let v1 = valoresOrdenados !! (mitad - 1)
                v2 = valoresOrdenados !! mitad
            in (v1 + v2) `div` 2
       else valoresOrdenados !! mitad

-- -----------------------------------------------------------------------------
-- | Calcula la moda (valor más frecuente) de una lista de números enteros.
--  Utiliza funciones de orden superior (group, sort, map, foldr).
-- 
-- ==== Parámetros
--
-- * 'valores' : Lista de valores enteros.
--
-- ==== Retorno
--
-- * Moda como número entero. Si hay empate, devuelve el menor valor.
--
-- ==== Ejemplo
--
-- >>> calcularModa [10, 20, 10, 30, 10]
-- 10
calcularModa :: [Int] -> Int
calcularModa [] = 0
calcularModa valores =
    let frecuencias = map (\grupo -> (head grupo, length grupo)) $ group $ sort valores
        (moda, _) = foldr1 (\(v1, f1) (v2, f2) -> 
                            if f1 > f2 || (f1 == f2 && v1 < v2) 
                            then (v1, f1) 
                            else (v2, f2)) frecuencias
    in moda

-- -----------------------------------------------------------------------------
-- | Completa los datos faltantes de cantidad en las ventas.
--  Considera cantidad = 0 como dato faltante.
--  Utiliza funciones de orden superior (map, filter).
-- 
-- ==== Parámetros
--
-- * 'ventas'   : Lista de ventas a procesar.
-- * 'tecnica'  : Técnica estadística a utilizar.
--
-- ==== Retorno
--
-- * 'ResultadoProcesamiento' con ventas procesadas e IDs modificados.
--
-- ==== Ejemplo
--
-- >>> completarCantidadFaltante [Venta 1 "2025-01-01" "P1" "Prod" "Cat" 0 100 100] Media
-- ResultadoProcesamiento {...}
completarCantidadFaltante :: [Venta] -> TecnicaEstadistica -> ResultadoProcesamiento
completarCantidadFaltante ventas tecnica =
    let cantidadesValidas = map cantidad $ filter (\v -> cantidad v > 0) ventas
        valorRelleno = case tecnica of
            Media   -> calcularMedia cantidadesValidas
            Mediana -> calcularMediana cantidadesValidas
            Moda    -> calcularModa cantidadesValidas
        
        (ventasActualizadas, idsModificados) = 
            foldr (\venta (acc, ids) -> 
                if cantidad venta == 0
                then let ventaActualizada = venta { cantidad = valorRelleno
                                                  , totalVenta = valorRelleno * precioUnitario venta }
                     in (ventaActualizada : acc, idVenta venta : ids)
                else (venta : acc, ids)
            ) ([], []) ventas
    
    in ResultadoProcesamiento 
        { ventasProcesadas = ventasActualizadas
        , registrosModificados = idsModificados
        , tecnicaUtilizada = "Cantidad - " ++ show tecnica
        }

-- -----------------------------------------------------------------------------
-- | Completa los datos faltantes de precio unitario en las ventas.
--  Considera precioUnitario = 0 como dato faltante.
--  Utiliza funciones de orden superior (map, filter, foldr).
-- 
-- ==== Parámetros
--
-- * 'ventas'   : Lista de ventas a procesar.
-- * 'tecnica'  : Técnica estadística a utilizar.
--
-- ==== Retorno
--
-- * 'ResultadoProcesamiento' con ventas procesadas e IDs modificados.
--
-- ==== Ejemplo
--
-- >>> completarPrecioUnitarioFaltante [Venta 1 "2025-01-01" "P1" "Prod" "Cat" 2 0 200] Media
-- ResultadoProcesamiento {...}
completarPrecioUnitarioFaltante :: [Venta] -> TecnicaEstadistica -> ResultadoProcesamiento
completarPrecioUnitarioFaltante ventas tecnica =
    let preciosValidos = map precioUnitario $ filter (\v -> precioUnitario v > 0) ventas
        valorRelleno = case tecnica of
            Media   -> calcularMedia preciosValidos
            Mediana -> calcularMediana preciosValidos
            Moda    -> calcularModa preciosValidos
        
        (ventasActualizadas, idsModificados) = 
            foldr (\venta (acc, ids) -> 
                if precioUnitario venta == 0
                then let ventaActualizada = venta { precioUnitario = valorRelleno
                                                  , totalVenta = cantidad venta * valorRelleno }
                     in (ventaActualizada : acc, idVenta venta : ids)
                else (venta : acc, ids)
            ) ([], []) ventas
    
    in ResultadoProcesamiento 
        { ventasProcesadas = ventasActualizadas
        , registrosModificados = idsModificados
        , tecnicaUtilizada = "Precio Unitario - " ++ show tecnica
        }

-- -----------------------------------------------------------------------------
-- | Elimina ventas duplicadas basándose en múltiples criterios.
--  Considera duplicadas las ventas con mismo ID, fecha, producto y cantidad.
--  Utiliza funciones de orden superior (groupBy, map, concat).
-- 
-- ==== Parámetros
--
-- * 'ventas' : Lista de ventas a procesar.
--
-- ==== Retorno
--
-- * 'ResultadoProcesamiento' con duplicados eliminados e IDs afectados.
--
-- ==== Ejemplo
--
-- >>> eliminarDuplicados [venta1, venta1, venta2]
-- ResultadoProcesamiento {...}
eliminarDuplicados :: [Venta] -> ResultadoProcesamiento
eliminarDuplicados ventas =
    let ventasOrdenadas = sortBy (compare `on` claveUnica) ventas
        gruposDuplicados = groupBy ((==) `on` claveUnica) ventasOrdenadas
        
        (ventasUnicas, idsEliminados) = 
            foldr (\grupo (acc, ids) -> 
                case grupo of
                    [] -> (acc, ids)
                    (primera:resto) -> 
                        let idsRestantes = map idVenta resto
                        in (primera : acc, idsRestantes ++ ids)
            ) ([], []) gruposDuplicados
    
    in ResultadoProcesamiento 
        { ventasProcesadas = ventasUnicas
        , registrosModificados = idsEliminados
        , tecnicaUtilizada = "Eliminación de duplicados"
        }
  where
    claveUnica venta = (idVenta venta, fecha venta, idProducto venta, cantidad venta)

-- -----------------------------------------------------------------------------
-- | Procesa completamente una lista de ventas aplicando todas las técnicas
--  de limpieza: completar datos faltantes y eliminar duplicados.
--  Utiliza composición de funciones y funciones de orden superior.
-- 
-- ==== Parámetros
--
-- * 'ventas'        : Lista de ventas a procesar.
-- * 'tecnicaCantidad' : Técnica para completar cantidad faltante.
-- * 'tecnicaPrecio'   : Técnica para completar precio faltante.
--
-- ==== Retorno
--
-- * Lista de 'ResultadoProcesamiento' con todos los pasos aplicados.
--
-- ==== Ejemplo
--
-- >>> procesarDatosCompleto ventas Media Mediana
-- [ResultadoProcesamiento {...}, ResultadoProcesamiento {...}, ...]
procesarDatosCompleto :: [Venta] -> TecnicaEstadistica -> TecnicaEstadistica -> [ResultadoProcesamiento]
procesarDatosCompleto ventas tecnicaCantidad tecnicaPrecio =
    let paso1 = completarCantidadFaltante ventas tecnicaCantidad
        paso2 = completarPrecioUnitarioFaltante (ventasProcesadas paso1) tecnicaPrecio
        paso3 = eliminarDuplicados (ventasProcesadas paso2)
    in [paso1, paso2, paso3]

-- -----------------------------------------------------------------------------
-- | Muestra un resumen del procesamiento aplicado.
--  Utiliza funciones de orden superior (map, foldr).
-- 
-- ==== Parámetros
--
-- * 'resultados' : Lista de resultados de procesamiento.
--
-- ==== Retorno
--
-- * String con resumen formateado del procesamiento.
--
-- ==== Ejemplo
--
-- >>> mostrarResumenProcesamiento [resultado1, resultado2]
-- "=== RESUMEN DE PROCESAMIENTO ===\n..."
mostrarResumenProcesamiento :: [ResultadoProcesamiento] -> String
mostrarResumenProcesamiento resultados =
    let encabezado = "=== RESUMEN DE PROCESAMIENTO DE DATOS ===\n\n"
        resumenPasos = map formatearPaso resultados
        totalModificados = length $ foldr (\resultado acc -> 
                                          registrosModificados resultado ++ acc) [] resultados
        pie = "\nTotal de registros procesados: " ++ show totalModificados ++ "\n"
    in encabezado ++ concat resumenPasos ++ pie
  where
    formatearPaso resultado =
        "Técnica: " ++ tecnicaUtilizada resultado ++ "\n" ++
        "Registros modificados: " ++ show (registrosModificados resultado) ++ "\n" ++
        "Cantidad de modificaciones: " ++ show (length $ registrosModificados resultado) ++ "\n\n"