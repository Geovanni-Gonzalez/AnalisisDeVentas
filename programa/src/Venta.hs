{-# LANGUAGE DeriveGeneric #-}
module Venta where  
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Data de Venta
data Venta = Venta {
    idVenta :: Int,
    fecha :: String,
    idProducto :: String,
    nombreProducto :: String,
    categoria :: String,
    cantidad :: Int,
    precioUnitario :: Int,
    totalVenta :: Int
} deriving (Show, Generic)

-- Instancias para JSON
instance ToJSON Venta
instance FromJSON Venta