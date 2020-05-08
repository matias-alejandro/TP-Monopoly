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

barata = Propiedad {
	nombrePropiedad = "propi",
	precio = 100
}

cara = Propiedad {
	nombrePropiedad = "propi",
	precio = 500
}

pepe = Persona {
	nombre = "Carolina",
	cantDinero = 500,
	tactica = "Accionista",
	propiedades = [barata,cara,cara,barata,barata,barata],
	acciones = [pasarPorElBanco]--,pagarAAccionistas]
}

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

-- subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o “Accionista” podrán ganar la propiedad.
-- Ganar implica restar el precio de la propiedad de su dinero y sumar la nueva adquisición a sus propiedades. 

pasarPorElBanco :: Accion
pasarPorElBanco persona = Persona (nombre persona) (cantDinero persona+40) "Comprador compulsivo" (propiedades persona) (acciones persona)

------

gritar :: Accion
gritar persona = Persona ("AHHHH"++(nombre persona)) (cantDinero persona) (tactica persona) (propiedades persona) (acciones persona)

------

enojarse :: Accion
enojarse persona = Persona (nombre persona) (cantDinero persona+50) (tactica persona) (propiedades persona) ((acciones persona)++[gritar])

------

subastar :: Accion
subastar persona = 

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
pagarAAccionistas persona | (esAccionista persona == True) = UnaPersona (nombre persona) (cantidadDeDinero persona + 200) (tactica persona) (propiedadesCompradas persona) (acciones persona)
                          | otherwise = UnaPersona (nombre persona) (cantidadDeDinero persona - 100) (tactica persona) (propiedadesCompradas persona) (acciones persona)