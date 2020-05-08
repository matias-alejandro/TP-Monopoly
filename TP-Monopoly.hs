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
	acciones = [pasarPorElBanco]--,pagarAAccionistas]
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
pasarPorElBanco persona = Persona (nombre persona) (cantDinero persona+40) "Comprador compulsivo" (propiedades persona) (acciones persona)

------

gritar :: Accion
gritar persona = Persona ("AHHHH"++(nombre persona)) (cantDinero persona) (tactica persona) (propiedades persona) (acciones persona)

------

enojarse :: Accion
enojarse persona = Persona (nombre persona) (cantDinero persona+50) (tactica persona) (propiedades persona) ((acciones persona)++[gritar])

------

tieneLaTactica :: Persona -> Bool
tieneLaTactica persona = tactica persona == "Oferente singular" || tactica persona == "Accionista" 

subastar :: Propiedad -> Accion
subastar propiedad persona
  | (tieneLaTactica persona == True) = Persona (nombre persona) (cantDinero persona - precio propiedad) (tactica persona) (propiedades persona ++ [propiedad]) (acciones persona)
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
cobrarAlquileres persona = Persona (nombre persona) (cantDinero persona+50) (tactica persona) (propiedades persona) (acciones persona)

------

esAccionista :: Persona -> Bool
esAccionista persona = tactica persona == "Accionista"

pagarAAccionistas :: Accion
pagarAAccionistas persona
  | (esAccionista persona == True) = Persona (nombre persona) (cantDinero persona + 200) (tactica persona) (propiedades persona) (acciones persona)
  | otherwise = Persona (nombre persona) (cantDinero persona - 100) (tactica persona) (propiedades persona) (acciones persona)
