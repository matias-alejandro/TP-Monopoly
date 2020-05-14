import Data.List
import Text.Show.Functions

type Accion = (Persona -> Persona)

data Propiedad = Propiedad {
  nombrePropiedad :: String,
  precio :: Int
} deriving (Show)

data Persona = Persona {
  nombre :: String,
  cantDinero :: Int,
  tactica :: String,
  propiedades :: [Propiedad],
  acciones :: [Accion]
} deriving (Show)

------

carolina = Persona {
  nombre = "Carolina",
  cantDinero = 500,
  tactica = "Accionista",
  propiedades = [],
  acciones = [pasarPorElBanco, pagarAAccionistas]
}

manuel = Persona {
  nombre = "Manuel",
  cantDinero = 500,
  tactica = "Oferente singular",
  propiedades = [],
  acciones = [pasarPorElBanco, enojarse]
}
------

alterarDinero :: Int -> Persona -> Persona
alterarDinero cantidad persona = persona {cantDinero = (+) cantidad (cantDinero persona) }

------

agregarAccion :: Persona -> Accion -> Persona
agregarAccion persona accion = persona {acciones = accion:acciones persona}

------

agregarPropiedad :: Persona -> Propiedad -> Persona
agregarPropiedad persona propiedad = persona {propiedades = propiedad:propiedades persona}

------

pasarPorElBanco :: Accion
pasarPorElBanco persona = alterarDinero 40 persona {tactica = "Comprador compulsivo"}

------

gritar :: Accion
gritar persona = persona {nombre = "AHHHH"++(nombre persona)}

------

enojarse :: Accion
enojarse persona = alterarDinero 50 $ agregarAccion persona gritar

------

comprarPropiedad :: Propiedad -> Persona -> Persona
comprarPropiedad propiedad persona = alterarDinero (-precio propiedad) $ agregarPropiedad persona propiedad

tieneLaTactica :: Persona -> Bool
tieneLaTactica persona = tactica persona == "Oferente singular" || tactica persona == "Accionista" 

subastar :: Propiedad -> Accion
subastar propiedad persona
  | tieneLaTactica persona = comprarPropiedad propiedad persona
  | otherwise = persona

------

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata propiedad = (>) 150 (precio propiedad)

valorDePropiedad :: Propiedad -> Int
valorDePropiedad propiedad
  | esPropiedadBarata propiedad = 10
  | otherwise = 20

gananciaPorPropiedad :: [Propiedad] -> [Int]
gananciaPorPropiedad = map valorDePropiedad

gananciaTotal :: Persona -> Int
gananciaTotal persona = sum $ gananciaPorPropiedad $ propiedades persona

cobrarAlquileres :: Accion
cobrarAlquileres persona = alterarDinero (gananciaTotal persona) persona

------

esAccionista :: Persona -> Bool
esAccionista persona = tactica persona == "Accionista"

pagarAAccionistas :: Accion
pagarAAccionistas persona
  | esAccionista persona = alterarDinero 200 persona
  | otherwise = alterarDinero (-100) persona

------

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor propiedad persona
  | cantDinero persona < precio propiedad = hacerBerrinchePor propiedad $ alterarDinero 10 $ gritar persona 
  | otherwise = comprarPropiedad propiedad persona

------

ultimaRonda :: Persona -> Accion
ultimaRonda persona = foldl (.) (head $ acciones persona) (tail $ acciones persona)

------

dineroAlAplicarSusAcciones :: Persona -> Int
dineroAlAplicarSusAcciones persona = cantDinero $ ultimaRonda persona persona

juegoFinal :: Persona -> Persona -> Persona
juegoFinal persona1 persona2
  | dineroAlAplicarSusAcciones persona1 > dineroAlAplicarSusAcciones persona2 = persona1
  | otherwise = persona2
