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
    ordenPreguntas :: [Int] -- Orden de las preguntas del test (permutaciones) | Cada elemento indica el indice de la pregunta en "preguntas" de Test
} deriving (Show)

{--
Ahora vamos a definir un tipo de datos para las respuestas.
Cada respuesta tiene un identificador del estudiante (En mi caso, voy a utilizar
el DNi de cada estudiante que será una cadena). Además de ello, necesitamos saber
el modelo de examen (que nos servirá para corregir las respuestas dadas). Por último,
necesitamos otra lista de respuestas introducidas por el usuario.

Seguiré la siguiente convención para los valores de las respuestas:
- 0 (respuesta no contestada)
- >0 (respuesta contestada).
--}

data RespuestaEstudiante = RespuestaEstudiante {
    identificador :: String,
    modelo :: Int,
    respuestas :: [Respuesta]
}

data Respuesta = Respuesta {
    valor :: Int
}

{--
Como hemos podido observar en la función anterior, necesitamos uan lista de preguntas
para poder comprobar si la lista de respuestas del usuario es correcta.

Por tanto, necesitamos obtener una lista de Preguntas para un modelo dado.
Como el modelo es una lista de enteros que representan las permutaciones
de preguntas, entonces tenemos que encontrar de devolver una lista de preguntas
del testo pero con el orden que tiene el modelo.

Para esto, necesitamos 3 funciones:
  1. pedirPreguntasDelModelo
     Recibe un test, un modelo de dicho test y devuelve una lista de preguntas.
     Para ello llama a cogerPreguntasDadoOrden y le pasa la lista de preguntas
     que tiene el test y el orden de preguntas del modelo de test.

  2. cogerOrdenPreguntas.
      Devuelve una lista de enteros, cuyos elementos representan el la pregunta
      a la que se refiere nuestro modelo.

  3. cogerPreguntasDadoOrden.
      Recibe una lista de preguntas y el orden de las preguntas del modelo.
      Por tanto, lo que devuelve es una lista de preguntas (que sigue el orden de nuestro modelo)

--}

pedirPreguntasDelModelo :: Test -> Modelo -> [Pregunta]
pedirPreguntasDelModelo (Test preguntas modelos) modelo =
   cogerPreguntasDadoOrden preguntas (cogerOrdenPreguntas modelo)

cogerOrdenPreguntas :: Modelo -> [Int]
cogerOrdenPreguntas (Modelo ordenPreguntas) = ordenPreguntas

-- va componiendo una lista de preguntas siguiente el orden dado
cogerPreguntasDadoOrden :: [Pregunta] -> [Int] -> [Pregunta]
cogerPreguntasDadoOrden [] _ = []
cogerPreguntasDadoOrden _ [] = []
cogerPreguntasDadoOrden (preguntas) (indice:restoIndices) =
    (preguntas !! (indice - 1)) : cogerPreguntasDadoOrden(preguntas restoIndices)-- coger la pregunta

{--
Para calcular la nota de un estudiante vamos a necesitar dos funciones:

1. notaEstudiante:
  Esta función calcula la nota final obtenida por el estudiante.
  Recibe 2 paramétros:
    - Una lista de Preguntas del test (que tienen el número de opciones y la respuesta válida).
    - Una lista de respuestas dadas por el estudiante.
  Devuelve:
    - La nota total obtenida por el estudiante. Para ello, en cada llamada
      recursiva coge la primera pregunta y la primera respuesta de ambas listas y
      llama a notaPregunta para obtener la puntuación para esa pregunta. Luego,
      sumará esa puntuación junto con el resto de la lista, por lo que hará una
      llamada recursiva a la misma función pero con el resto de elementos de la lista.

2. notaPregunta:
  Recibe 2 paramétros:
    - Una pregunta del test (con el número de respuesta correcta y numero de opciones)
    - Una respuesta del estudiante (que será un valor
      entero desde 0 a N - siendo N el número máximo de opciones).
  Devuelve:
    - La puntuación obtenida para esa pregunta. Puede ser:
        * 1 si es correcta
        * 0 si no la ha contestado
        * -1/(N-1) si es incorrecta
--}

notaEstudiante :: [Pregunta] -> [Respuesta] -> Float
notaEstudiante [] _ = 0
notaEstudiante _ [] = 0
notaEstudiante (pregunta:preguntas) (respuesta:respuestas) =
    notaPregunta(pregunta respuesta) + notaEstudiante(preguntas respuestas)

notaPregunta :: Pregunta -> Respuesta -> Float
notaPregunta (Pregunta respuestaCorrecta opciones) (Respuesta respuestaEstudiante)
        | respuestaEstudiante == 0                  = 0 -- Respuesta en Blanco
        | respuestaEstudiante == respuestaCorrecta  = 1 -- Respuesta correcta
        | otherwise                                 = (-1) / (opciones - 1) -- Respuesta incorrecta, fórmula -1/(N-1) [Siendo N el número de alternativas]
