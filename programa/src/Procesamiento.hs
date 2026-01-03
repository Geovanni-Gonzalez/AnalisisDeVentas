{-# LANGUAGE OverloadedStrings #-}
-- |
-- Módulo      : Procesamiento
-- Descripción : Funciones para procesamiento y limpieza de datos de ventas
-- Licencia    : MIT
-- Autor       : Gerny Diaz
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
       then let valorIzq = valoresOrdenados !! (mitad - 1)
                valorDer = valoresOrdenados !! mitad
            in (valorIzq + valorDer) `div` 2
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
        (moda, _) = foldr1 (\(valorCandidato, frecuenciaCandidato) (valorActualModa, frecuenciaActualModa) -> 
                                if frecuenciaCandidato > frecuenciaActualModa || 
                                   (frecuenciaCandidato == frecuenciaActualModa && valorCandidato < valorActualModa) 
                                then (valorCandidato, frecuenciaCandidato) 
                                else (valorActualModa, frecuenciaActualModa)) frecuencias
    in moda

-- -----------------------------------------------------------------------------
-- | Función auxiliar genérica para completar datos faltantes.
completarDatoGenerico 
    :: (Venta -> Int)            -- ^ Selector del campo (ej. cantidad)
    -> (Venta -> Int -> Venta)   -- ^ Actualizador del campo
    -> String                    -- ^ Etiqueta para el reporte
    -> [Venta] 
    -> TecnicaEstadistica 
    -> ResultadoProcesamiento
completarDatoGenerico getField setField label ventas tecnica =
    let valoresValidos = map getField $ filter (\v -> getField v > 0) ventas
        valorRelleno = case tecnica of
            Media   -> calcularMedia valoresValidos
            Mediana -> calcularMediana valoresValidos
            Moda    -> calcularModa valoresValidos
        
        (ventasActualizadas, idsModificados) = 
            foldr (\ventaActual (accVentas, accIds) -> 
                if getField ventaActual == 0
                then let ventaNueva = setField ventaActual valorRelleno
                         -- Recalcular total si es necesario, asumimos que el setter lo maneja o lo hacemos aquí?
                         -- Los setters originales actualizaban totalVenta. 
                         -- Lo manejaremos pasando una función que actualice todo.
                         ventaCompleta = ventaNueva { totalVenta = cantidad ventaNueva * precioUnitario ventaNueva }
                     in (ventaCompleta : accVentas, idVenta ventaActual : accIds)
                else (ventaActual : accVentas, accIds)
            ) ([], []) ventas
            
    in ResultadoProcesamiento 
        { ventasProcesadas = ventasActualizadas
        , registrosModificados = idsModificados
        , tecnicaUtilizada = label ++ " - " ++ show tecnica
        }

-- -----------------------------------------------------------------------------
-- | Completa los datos faltantes de cantidad en las ventas.
completarCantidadFaltante :: [Venta] -> TecnicaEstadistica -> ResultadoProcesamiento
completarCantidadFaltante ventas = completarDatoGenerico cantidad (\v n -> v { cantidad = n }) "Cantidad" ventas

-- -----------------------------------------------------------------------------
-- | Completa los datos faltantes de precio unitario en las ventas.
completarPrecioUnitarioFaltante :: [Venta] -> TecnicaEstadistica -> ResultadoProcesamiento
completarPrecioUnitarioFaltante ventas = completarDatoGenerico precioUnitario (\v n -> v { precioUnitario = n }) "Precio Unitario" ventas

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
        
        (ventasSinDuplicados, idsVentasEliminadas) = 
            foldr (\grupoDuplicados (acumuladorVentasUnicas, acumuladorIdsEliminados) -> 
                case grupoDuplicados of
                    [] -> (acumuladorVentasUnicas, acumuladorIdsEliminados)
                    (primera:ventasDuplicadas) -> 
                        let idsVentasDuplicadas = map idVenta ventasDuplicadas
                        in (primera : acumuladorVentasUnicas, idsVentasDuplicadas ++ acumuladorIdsEliminados)
            ) ([], []) gruposDuplicados
    
    in ResultadoProcesamiento 
        { ventasProcesadas = ventasSinDuplicados
        , registrosModificados = idsVentasEliminadas
        , tecnicaUtilizada = "Eliminación de duplicados"
        }
  where
    claveUnica venta = idVenta venta

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
        totalModificados = length $ foldr (\resultado acumuladorIds -> 
                                          registrosModificados resultado ++ acumuladorIds) [] resultados
        textFinal = "\nTotal de registros procesados: " ++ show totalModificados ++ "\n"
    in encabezado ++ concat resumenPasos ++ textFinal
  where
    formatearPaso resultado =
        "Técnica: " ++ tecnicaUtilizada resultado ++ "\n" ++
        "Registros modificados: " ++ show (registrosModificados resultado) ++ "\n" ++
        "Cantidad de modificaciones: " ++ show (length $ registrosModificados resultado) ++ "\n\n"
