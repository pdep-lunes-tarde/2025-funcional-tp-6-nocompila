module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | 
    Papas | PatiVegano | PanIntegral | BaconDeTofu
    deriving (Eq, Show)


precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente Papas = 10
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3
precioIngrediente BaconDeTofu = 12

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


precioFinal :: Hamburguesa -> Number
precioFinal (Hamburguesa precioBase ingredientes) = precioBase + sum (map precioIngrediente ingredientes)

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    | elem PatiVegano (ingredientes hamburguesa) = agregarIngrediente PatiVegano hamburguesa
    | elem Carne (ingredientes hamburguesa) = agregarIngrediente Carne hamburguesa
    | elem Pollo (ingredientes hamburguesa) = agregarIngrediente Pollo hamburguesa
    | otherwise = hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente nuevoIngrediente hamburguesa = 
    hamburguesa { ingredientes = nuevoIngrediente : ingredientes hamburguesa }

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentajeDescuento hamburguesa = 
    hamburguesa { precioBase = precioBase hamburguesa * (100 - porcentajeDescuento)/100}  

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = (agregarIngrediente Papas . descuento 30) hamburguesa

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa =
    hamburguesa { ingredientes = map cambiarIngredienteAVegano (ingredientes hamburguesa)}

cambiarIngredienteAVegano :: Ingrediente -> Ingrediente
cambiarIngredienteAVegano Carne = PatiVegano
cambiarIngredienteAVegano Pollo = PatiVegano
cambiarIngredienteAVegano Cheddar = QuesoDeAlmendras
cambiarIngredienteAVegano Panceta = BaconDeTofu
cambiarIngredienteAVegano ingrediente = ingrediente

cambiarPan :: Ingrediente -> Ingrediente
cambiarPan Pan = PanIntegral

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa =
    hamburguesa {ingredientes = map cambiarPan (ingredientes hamburguesa)}