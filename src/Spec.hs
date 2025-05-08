module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

cuartoDeLibraDePollo :: Hamburguesa
cuartoDeLibraDePollo = Hamburguesa 20 [Pan, Pollo, Cheddar, Pan]

cuartoDeLibraVeggie :: Hamburguesa
cuartoDeLibraVeggie = hacerVeggie cuartoDeLibra

pdepBurguer :: Hamburguesa
pdepBurguer = agrandar . agrandar . 
              agregarIngrediente Panceta . agregarIngrediente Cheddar . 
              descuento 20 
              $ cuartoDeLibra 

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Carne . agregarIngrediente Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

dobleCuartoVeggie :: Hamburguesa
dobleCuartoVeggie = cambiarPanDePati . hacerVeggie $ dobleCuarto

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do
        describe "Agrandar" $ do
            it "al agrandar una hamburguesa se le agrega nuevamente su ingrediente base" $ do
                agrandar cuartoDeLibra `shouldBe` Hamburguesa 20 [Carne, Pan, Carne, Cheddar, Pan]
                agrandar cuartoDeLibraDePollo `shouldBe` Hamburguesa 20 [Pollo, Pan, Pollo, Cheddar, Pan]
                agrandar cuartoDeLibraVeggie `shouldBe` Hamburguesa 20 [PatiVegano, Pan, PatiVegano, QuesoDeAlmendras, Pan]
            it "al agrandar una hamburguesa con Carne y Pollo se le agrega alguno de estos dos" $ do
                agrandar (Hamburguesa 20 [Pan, Carne, Pollo, Cheddar, Pan]) `shouldBe` Hamburguesa 20 [Carne, Pan, Carne, Pollo, Cheddar, Pan]

        describe "agregarIngrediente" $ do
            it "Al agregar un ingrediente a una Hamburguesa la hamburguesa cambia" $ do
                agregarIngrediente Curry dobleCuarto `shouldBe` Hamburguesa 20 [Curry, Carne, Cheddar, Pan, Carne, Cheddar, Pan]
                agregarIngrediente QuesoDeAlmendras cuartoDeLibraDePollo `shouldBe` Hamburguesa 20 [QuesoDeAlmendras, Pan, Pollo, Cheddar, Pan]
                agregarIngrediente BaconDeTofu cuartoDeLibraVeggie `shouldBe`  Hamburguesa 20 [BaconDeTofu, Pan, PatiVegano, QuesoDeAlmendras, Pan]
            it "Al agregar un ingrediente a una Hamburguesa la hamburguesa aumenta su precio" $ do
                precioFinal (agregarIngrediente QuesoDeAlmendras dobleCuarto) `shouldBe` 99

        describe "descuento" $ do
            it "Al aplicarle un descuento a una hamburguesa se reduce su precio" $ do
                descuento 20 cuartoDeLibra `shouldBe` Hamburguesa 16 [Pan, Carne, Cheddar, Pan]
                descuento 0 dobleCuarto `shouldBe` dobleCuarto
            
        describe "delDia" $ do
            it "Al aplicarle la promo del dia a una hamburguesa, se descuenta un 30% y se le agregan papas" $ do
                delDia cuartoDeLibra `shouldBe` Hamburguesa 14 [Papas, Pan, Carne, Cheddar, Pan]
                delDia dobleCuarto `shouldBe`  Hamburguesa 14 [Papas, Carne, Cheddar, Pan, Carne, Cheddar, Pan]
        
        describe "precioFinal" $ do
            it "el precio final de una pdepBurger deberia ser 110" $ do
                precioFinal pdepBurguer `shouldBe` 110
            it "el precio final de una doble cuarto deberia ser 84" $ do
                precioFinal dobleCuarto `shouldBe` 84
            it "el precio final de una bigPdep deberia ser 89" $ do
                precioFinal bigPdep `shouldBe` 89
            it "el precio final de una doble cuarto del dia deberia ser 88" $ do
                precioFinal (delDia dobleCuarto) `shouldBe` 88

        describe "hacerVeggie" $ do
            it "Convertir una hamburguesa en veggie cambia sus ingredientes por ingredientes veganos" $ do
                hacerVeggie cuartoDeLibra `shouldBe` Hamburguesa 20 [Pan, PatiVegano, QuesoDeAlmendras, Pan]
                hacerVeggie bigPdep `shouldBe` Hamburguesa 20 [Curry,PatiVegano,QuesoDeAlmendras,Pan,PatiVegano,QuesoDeAlmendras,Pan]
            it "Hacer veggie una hamburguesa veggie no la modifica" $ do
                hacerVeggie cuartoDeLibraVeggie `shouldBe` cuartoDeLibraVeggie
        
        describe "cambiarPanDePati" $ do
            it "cambia el pan de pati por pan integral en una hamburguesa" $ do
                cambiarPanDePati cuartoDeLibra `shouldBe` Hamburguesa 20 [PanIntegral, Carne, Cheddar, PanIntegral]
            it "si la hamburguesa no tiene pan de pati no la modifica" $ do
                cambiarPanDePati dobleCuartoVeggie `shouldBe` dobleCuartoVeggie