---------------------------------------
-- EJEMPLOS ---------------------------
---------------------------------------

preguntas = [
      (Pregunta 1 2)
    , (Pregunta 1 2)
    , (Pregunta 1 2)
    , (Pregunta 1 2)
    , (Pregunta 1 2) ]

modelos = [
      (Modelo [1, 2, 3, 4, 5])
    , (Modelo [5, 4, 3, 2, 1])]

test = (Test preguntas modelos)

respuestas = [
      (RespuestaEstudiante "414992032-W" 1 [(Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1)])
    , (RespuestaEstudiante "414992032-W" 1 [(Respuesta 2), (Respuesta 2), (Respuesta 2), (Respuesta 2), (Respuesta 2)])
    , (RespuestaEstudiante "414992032-W" 1 [(Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1)])]

correccion = (corrige_todos test respuestas)
correccion_individual = (corrige test (respuestas !! 1))

estadisticas_totales = estadisticas test respuestas

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
    respuestas_estudiante :: [Respuesta]
} deriving (Show)

data Respuesta = Respuesta {
    valor :: Int
}  deriving (Show)

{--
En nuestro programa tenemos que poder corregir los tests de nuestros estudiantes.
Para ello necesitamos tener una función que reciba el test y las respuestas dadas
por el estudiante.

Como cada estudiante puede responder un modelo de examen diferente. Entonces
vamos a necesitar por un lado obtener una lista de preguntas que sigan el orden
del modelo de examen que está respondiendo el usuario para posteriormente utilizar
nuestra función notaEstudiante que recibe una lista de preguntas (las del modelo
del examen) y una lista de respuestas dadas por el usuario y devuelve la nota obtenida.

Con el resultado de dicha función, vamos a poder obtener la corrección del usuario:
Tanto: identificador del alumno, puntuaciónTotal y puntuación sobre 10.
--}

data Correccion = Correccion {
    identificadorAlumno :: String,
    puntuacionTotal :: Float,
    puntuacionSobre10 :: Float

    -- correctas :: [Int],
    -- erroneas :: [Int],
    -- blancas :: [Int]

}  deriving (Show)

corrige_todos :: Test -> [RespuestaEstudiante] -> [Correccion]
corrige_todos _ [] = []
corrige_todos test (x: xs) =
    corrige test x : corrige_todos test xs

corrige :: Test -> RespuestaEstudiante -> Correccion
corrige (Test preguntas modelos) (RespuestaEstudiante identificador indiceModelo respuestas) =
        (Correccion
            identificador
            (notaEstudiante (cogerPreguntasDelModelo preguntas (modelos !! (indiceModelo-1))) respuestas)
            (puntuacion10 (length preguntas) (notaEstudiante (cogerPreguntasDelModelo preguntas (modelos !! (indiceModelo-1))) respuestas))
        )

{--
Métodos para coger atributos de Corrección.
--}

cogerPuntuacionTotal :: Correccion -> Float
cogerPuntuacionTotal (Correccion _ puntuacion _) = puntuacion

cogerPuntuacionSobre10 :: Correccion -> Float
cogerPuntuacionSobre10 (Correccion _ _ puntuacion10) = puntuacion10

{--
  Funciones auxiliares para las estadísticas.
   1. puntuacionMedia = coger todas las correcciones, quedarte con el campo
      puntuacionTotal y sumarlo todos
--}

calcularPuntuacionMedia :: [Correccion] -> Float
calcularPuntuacionMedia correccion =
    (sumarPuntuaciones correccion) / fromIntegral( length(correccion) )

sumarPuntuaciones :: [Correccion] -> Float
sumarPuntuaciones [] = 0.0
sumarPuntuaciones (x:xs) =
    (cogerPuntuacionSobre10 x) + sumarPuntuaciones xs

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

3. NotaSobre10:
  Transforma la nota obtenida en una nota sobre 10.
  Para ello necesita el número de preguntas y la nota sacada para luego
  aplicar una regla de 3.
--}

notaEstudiante :: [Pregunta] -> [Respuesta] -> Float
notaEstudiante [] _ = 0.0
notaEstudiante _ [] = 0.0
notaEstudiante (pregunta:preguntas) (respuesta:respuestas) =
    (notaPregunta pregunta respuesta) + (notaEstudiante preguntas respuestas)

notaPregunta :: Pregunta -> Respuesta -> Float
notaPregunta (Pregunta respuestaCorrecta opciones) (Respuesta respuestaEstudiante)
        | respuestaEstudiante == 0                  = 0.0 -- Respuesta en Blanco
        | respuestaEstudiante == respuestaCorrecta  = 1.0 -- Respuesta correcta
        | otherwise                                 = (-1.0) / (fromIntegral opciones - 1.0) -- Respuesta incorrecta, fórmula -1/(N-1) [Siendo N el número de alternativas]

puntuacion10 :: Int -> Float -> Float
puntuacion10 numeroPreguntas notaObtenida
        -- Número máximo de puntos posibles sería el numero de preguntas (ganas 1 punto por cada pregunta)
        -- maxPuntosPosible ______ 10 nota maxima
        -- puntosObtenidos ______ X nota sobre 10 --}
        | notaTotal <= 0      = 0
        | otherwise           = notaTotal
        where notaTotal = (10.0 * notaObtenida) / fromIntegral numeroPreguntas

{--
Como hemos podido observar en la función anterior, necesitamos uan lista de preguntas
para poder comprobar si la lista de respuestas del usuario es correcta.

Por tanto, necesitamos obtener una lista de Preguntas para un modelo dado.
Como el modelo es una lista de enteros que representan las permutaciones
de preguntas, entonces tenemos que encontrar de devolver una lista de preguntas
del testo pero con el orden que tiene el modelo.

Para esto, necesitamos 3 funciones:
  1. cogerPreguntasDelModelo
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

cogerPreguntasDelModelo :: [Pregunta] -> Modelo -> [Pregunta]
cogerPreguntasDelModelo preguntasDelTest modelo =
   cogerPreguntasDadoOrden preguntasDelTest (cogerOrdenPreguntas modelo)

-- cogerPreguntasDelModelo :: Test -> Modelo -> [Pregunta]
-- cogerPreguntasDelModelo (Test preguntas modelos) modelo =
--    cogerPreguntasDadoOrden preguntas (cogerOrdenPreguntas modelo)

cogerOrdenPreguntas :: Modelo -> [Int]
cogerOrdenPreguntas (Modelo ordenPreguntas) = ordenPreguntas

-- va componiendo una lista de preguntas siguiente el orden dado
cogerPreguntasDadoOrden :: [Pregunta] -> [Int] -> [Pregunta]
cogerPreguntasDadoOrden [] _ = []
cogerPreguntasDadoOrden _ [] = []
cogerPreguntasDadoOrden (preguntas) (indice:restoIndices) =
    (preguntas !! (indice - 1)) : (cogerPreguntasDadoOrden preguntas restoIndices)-- coger la pregunta

{--
Estadísticas
pregunta mas veces (respectivamente menos veces) dejada en blanco.
--}

data Estadisticas = Estadisticas {
    puntuacionMedia :: Float,
    mediaPreguntasRespondidas :: Float,

    numeroSuspensos :: Int, -- nota < 5
    numeroAprobados :: Int, -- 5 <= nota < 7
    numeroNotables  :: Int, -- 7 <= nota < 9
    numeroSobresalientes :: Int, -- 9 <= nota

    frecAbsRespuestasCorrectas :: Float,
    frecRelRespuestasCorrectas :: Float,
    frecAbsRespuestasErroneas :: Float,
    frecRelRespuestasErroneas :: Float,
    frecAbsRespuestasBlancos :: Float,
    frecRelRespuestasBlancos :: Float,

    preguntaMejorResultado :: Int,
    preguntaPeorResultado :: Int,

    preguntaMasContestada :: Int,
    preguntaMenosContestada :: Int

} deriving (Show)

estadisticas :: Test -> [RespuestaEstudiante] -> Estadisticas
estadisticas test respuestas =
    (Estadisticas
        (calcularPuntuacionMedia correcciones)
        10.0

        0
        0
        0
        0

        0.0
        0.0
        0.0
        0.0
        0.0
        0.0

        3
        1

        3
        1
    )
    where correcciones = (corrige_todos test respuestas)
