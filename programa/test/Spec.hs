{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Venta
import Estadisticas
import Procesamiento
import Data.Maybe (isJust, fromJust)

main :: IO ()
main = hspec $ do
  describe "Venta.hs" $ do
    it "Calculates total sales correctly" $ do
      let sales = [Venta 1 "2025-01-01" "P1" "A" "Cat1" 1 100 100, Venta 2 "2025-01-02" "P2" "B" "Cat1" 2 50 100]
      totalDeVentas sales `shouldBe` 200

    it "Groups by trimester correctly" $ do
      obtenerTrimestre "2025-01-15" `shouldBe` "2025-T1"
      obtenerTrimestre "2025-04-10" `shouldBe` "2025-T2"
      obtenerTrimestre "2025-10-01" `shouldBe` "2025-T4"

  describe "Estadisticas.hs" $ do
    let sales = [ Venta 1 "2025-01-01" "P1" "ProdA" "Elec" 10 100 1000
                , Venta 2 "2025-01-02" "P2" "ProdB" "Home" 5 50 250
                , Venta 3 "2025-01-03" "P1" "ProdA" "Elec" 2 100 200
                ]
    
    it "Identifies Top Categories by Sales Amount" $ do
      let top = top5CategoriasMayoresVentas sales
      head top `shouldBe` ("Elec", 1200)

    it "Identifies Best Selling Product by Quantity" $ do
      let best = productoMasVendido sales
      best `shouldBe` Just ("ProdA", 12)

  describe "Procesamiento.hs" $ do
    it "Completes missing price with Mean" $ do
      let raw = [ Venta 1 "2025-01-01" "P1" "A" "C" 1 100 100
                , Venta 2 "2025-01-02" "P2" "B" "C" 1 200 200
                , Venta 3 "2025-01-03" "P3" "C" "C" 1 0 0 -- Missing
                ]
      -- Mean of 100 and 200 is 150
      let result = completarPrecioUnitarioFaltante raw Media
      let processed = ventasProcesadas result
      let fixedSale = last processed -- Assuming order preservation or check ID 3
      let fixedList = filter (\v -> idVenta v == 3) processed
      precioUnitario (head fixedList) `shouldBe` 150

    it "Removes duplicates" $ do
      let dups = [ Venta 1 "2025-01-01" "P1" "A" "C" 1 100 100
                 , Venta 1 "2025-01-01" "P1" "A" "C" 1 100 100 ]
      let res = eliminarDuplicados dups
      length (ventasProcesadas res) `shouldBe` 1
