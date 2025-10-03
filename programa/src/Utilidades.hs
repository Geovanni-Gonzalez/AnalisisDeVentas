{-# LANGUAGE OverloadedStrings #-}
module Utilidades where
import System.Directory (doesFileExist)
import System.IO
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as B
import Venta

cargarVentasDeArchivo :: FilePath -> IO (Either String [Venta])
cargarVentasDeArchivo ruta = do
    contenido <- B.readFile ruta
    let resultado = eitherDecode contenido :: Either String [Venta]
    return resultado


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