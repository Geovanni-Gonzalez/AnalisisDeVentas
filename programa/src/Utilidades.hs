{-# LANGUAGE OverloadedStrings #-}
module Utilidades where
import System.Directory (doesFileExist)
import System.IO
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import Venta

cargarVentas :: FilePath -> IO (Either String [Venta])
cargarVentas ruta = do
    contenido <- B.readFile ruta
    let resultado = eitherDecode contenido :: Either String [Venta]
    return resultado



