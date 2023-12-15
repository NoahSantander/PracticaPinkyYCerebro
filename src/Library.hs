module Library where
import PdePreludat

-- Defino mis alias
type CoeficienteIntelectual = Number
type Especie = String
type Capacidad = String
type Capacidades = [Capacidad]
type Cantidad = Number
type Transformacion = Animal -> Animal
type Transformaciones = [Transformacion] 
type CondicionAnimal = Animal -> Bool
type Experimento = Animal -> Transformaciones -> CondicionAnimal -> Bool
type Animales = [Animal]

-- Defino mis tipos
data Animal = UnAnimal {
    coeficienteIntelectual :: CoeficienteIntelectual,
    especie :: Especie, 
    capacidades :: Capacidades
} deriving Show

-- Inicializo algunos animales
perro = UnAnimal 10 "Perro" ["Ladrar", "Comer"]
gato = UnAnimal 15 "Perro" ["Maullar", "Comer"]

-- Defino las siguientes transformaciones de animales
inteligenciaSuperior :: Cantidad -> Transformacion
inteligenciaSuperior cantidad animal = animal {coeficienteIntelectual = coeficienteIntelectual animal + cantidad}

pinkificar :: Transformacion
pinkificar animal = animal {capacidades = []}

esAnimal :: Especie -> CondicionAnimal
esAnimal especieAnimal = (==especieAnimal).especie

tieneCoeficienteMayorA :: CoeficienteIntelectual -> CondicionAnimal
tieneCoeficienteMayorA coeficiente = (>coeficiente).coeficienteIntelectual

agregarCapacidad :: Capacidad -> Transformacion
agregarCapacidad capacidad animal = animal {capacidades = capacidad:(capacidades animal)}

superPoderes :: Transformacion
superPoderes animal 
    | esAnimal "elefante" animal = agregarCapacidad "no tenerle miedo a los ratones" animal
    | esAnimal "raton" animal && tieneCoeficienteMayorA 100 animal = agregarCapacidad "hablar" animal
    | otherwise = animal

-- Defino si un animal cumple ciertas propiedades
tieneCapacidad :: Capacidad -> CondicionAnimal
tieneCapacidad capacidad = (elem capacidad).capacidades

antropomorfico :: CondicionAnimal
antropomorfico animal = tieneCapacidad "hablar" animal && tieneCoeficienteMayorA 60 animal

esVocal :: Char -> Bool
esVocal = flip elem ['a', 'e', 'i', 'o', 'u']

esPalabraPinkiesca :: String -> Bool
esPalabraPinkiesca palabra = ((<=4).length) palabra && any (esVocal) palabra

pinkiesco :: Capacidad -> Bool
pinkiesco capacidad = ((=="hacer ").(take 6)) capacidad && esPalabraPinkiesca (drop 6 capacidad)

cantidadDeSonidosPinkiescoMayorA :: Cantidad -> Capacidades -> Bool
cantidadDeSonidosPinkiescoMayorA cantidad capacidades = ((>cantidad).length.(filter (pinkiesco))) capacidades

noTanCuerdo :: CondicionAnimal
noTanCuerdo animal = cantidadDeSonidosPinkiescoMayorA 2 (capacidades animal)

-- Defino los experimentos
experimentoExitoso :: Experimento 
experimentoExitoso animal transformaciones condicion = condicion (foldr ($) animal $ transformaciones)

realizarExperimento :: Animal -> Transformaciones -> Animal
realizarExperimento animal transformaciones = foldr ($) animal $ transformaciones

-- experimentoExitoso (UnAnimal 17 "raton" ["estruenglonir el mundo", "hacer planes desalmados"]) [pinkificar, (inteligenciaSuperior 10), superPoderes] antropomorfico

-- Defino los siguientes informes
tieneAlgunaDeLasCapacidades :: Capacidades -> Animal -> Bool
tieneAlgunaDeLasCapacidades capacidades animal = any (flip tieneCapacidad animal) capacidades 

tieneLasCapacidades :: Capacidades -> Animal -> Bool
tieneLasCapacidades capacidades animal = all (flip tieneCapacidad animal) capacidades

informeCoeficientesIntelectuales :: Animales -> Capacidades -> Transformaciones -> [CoeficienteIntelectual]
informeCoeficientesIntelectuales animales capacidades transformaciones = map (coeficienteIntelectual) (filter (tieneAlgunaDeLasCapacidades capacidades) (map (flip realizarExperimento transformaciones) animales))

informeEspecies :: Animales -> Capacidades -> Transformaciones -> [Especie]
informeEspecies animales capacidades transformaciones = map (especie) (filter (tieneLasCapacidades capacidades) (map (flip realizarExperimento transformaciones) animales))

informeCapacidades :: Animales -> Capacidades -> Transformaciones -> [Number]
informeCapacidades animales capacidadesRequisito transformaciones = map (length.capacidades) (filter (not.tieneAlgunaDeLasCapacidades capacidadesRequisito) (map (flip realizarExperimento transformaciones) animales))

-- Si aparece un animal con infinitas capacidades, se lo puede pinkificar, pero no darle super poderes porque la funcion diverge