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

getIngredienteBase :: Hamburguesa -> Ingrediente
getIngredienteBase (Hamburguesa _ ingredientes)
    | elem PatiVegano ingredientes = PatiVegano
    | elem Carne ingredientes = Carne
    | elem Pollo ingredientes = Pollo

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa = agregarIngrediente (getIngredienteBase hamburguesa) hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente nuevoIngrediente hamburguesa = 
    hamburguesa { ingredientes = nuevoIngrediente : ingredientes hamburguesa }

porcentaje :: Number -> Number
porcentaje unNumero = (100 - unNumero)/100

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentajeDescuento hamburguesa = 
    hamburguesa { precioBase = precioBase hamburguesa * porcentaje porcentajeDescuento}  

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = agregarIngrediente Papas . descuento 30 $ hamburguesa

cambiarIngredienteAVegano :: Ingrediente -> Ingrediente
cambiarIngredienteAVegano Carne = PatiVegano
cambiarIngredienteAVegano Pollo = PatiVegano
cambiarIngredienteAVegano Cheddar = QuesoDeAlmendras
cambiarIngredienteAVegano Panceta = BaconDeTofu
cambiarIngredienteAVegano ingrediente = ingrediente

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa =
    hamburguesa { ingredientes = map cambiarIngredienteAVegano (ingredientes hamburguesa)}

cambiarPan :: Ingrediente -> Ingrediente
cambiarPan Pan = PanIntegral
cambiarPan ingrediente = ingrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa =
    hamburguesa { ingredientes = map cambiarPan (ingredientes hamburguesa)}