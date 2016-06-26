---------------------------------------
-- EJEMPLOS ---------------------------
---------------------------------------

p1 = (Pregunta 2 5)
p2 = (Pregunta 1 3)
preguntas = [p1, p2]

m1 = (Modelo [1, 2])
m2 = (Modelo [2, 1])
modelos = [m1, m2]

test = Test preguntas modelos

---------------------------------------
-- DATA STRUCTURES --------------------
---------------------------------------

{--
Cada Test tiene una lista de Preguntas y otra lista de Modelos de examen.
Una pregunta tiene el número de opciones disponibles y el índice de la respuesta.

Un modelo es una permutación de las preguntas. Esto significa, que cada Modelo
tiene una lista de enteros llamada "orden". La misión de esta lista es
reflejar las permutaciones posibles. Cada elemento de la lista hace referencia
a la lista "preguntas".

Por tanto, tener un modelo [1, 2, 3] significa,
que las preguntas están ordenadas. Si tenemos un modelo como [3, 2, 1],
entonces la primera pregunta que leerá ese estudiante, será la tercera
prefunta de la lista "preguntas".
--}

data Test = Test {
    preguntas_examen :: [Pregunta],
    modelos_examen :: [Modelo]
} deriving (Show)

data Pregunta = Pregunta {
    respuesta :: Int,
    opciones  :: Int
} deriving (Show)

data Modelo = Modelo {
    orden :: [Int] -- Orden de las preguntas del test (permutaciones) | Cada elemento indica el indice de la pregunta en "preguntas" de Test
} deriving (Show)
