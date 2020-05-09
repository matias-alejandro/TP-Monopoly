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

pasarPorElBanco :: Accion
pasarPorElBanco persona = persona {cantDinero = cantDinero persona+40, tactica = "Comprador compulsivo"}

------

gritar :: Accion
gritar persona = persona {nombre = "AHHHH"++(nombre persona)}

------

enojarse :: Accion
enojarse persona = persona {cantDinero = cantDinero persona+50, acciones = acciones persona++[gritar]}

------

tieneLaTactica :: Persona -> Bool
tieneLaTactica persona = tactica persona == "Oferente singular" || tactica persona == "Accionista" 

subastar :: Propiedad -> Accion
subastar propiedad persona
  | tieneLaTactica persona = persona {cantDinero = cantDinero persona - precio propiedad, propiedades = propiedades persona ++ [propiedad]}
  | otherwise = persona

------

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata propiedad = (>) 150 (precio propiedad)

cantPropiedadesSegun :: Persona -> (Propiedad -> Bool) -> Int
cantPropiedadesSegun persona condicion = length.filter (==True) $ map condicion (propiedades persona)

sumarPorPropiedad :: Persona -> Int
sumarPorPropiedad persona = 10 * (cantPropiedadesSegun persona esPropiedadBarata) + 20 * (cantPropiedadesSegun persona (not.esPropiedadBarata))

cantTotalDinero :: Persona -> Int
cantTotalDinero persona = (cantDinero persona) + (sumarPorPropiedad persona)

cobrarAlquileres :: Accion
cobrarAlquileres persona = persona {cantDinero = cantDinero persona+50}

------

esAccionista :: Persona -> Bool
esAccionista persona = tactica persona == "Accionista"

pagarAAccionistas :: Accion
pagarAAccionistas persona
  | (esAccionista persona == True) = persona {cantDinero = cantDinero persona + 200}
  | otherwise = persona {cantDinero = cantDinero persona - 100}
