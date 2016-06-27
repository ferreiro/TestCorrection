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
      (RespuestaEstudiante "ABC-W" 1 [(Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1)])
    , (RespuestaEstudiante "WWW-D" 1 [(Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 0)])
    , (RespuestaEstudiante "DSC-W" 1 [(Respuesta 0), (Respuesta 1), (Respuesta 0), (Respuesta 0), (Respuesta 0)])
    , (RespuestaEstudiante "414992032-W" 1 [(Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1)])]

_correcciones = (corrige_todos test respuestas)
_estadisticas = estadisticas test respuestas

mostrar = mostrarResultadosEstadisticas _estadisticas

------------------------------------------------------------------
-- PARTE OPCIONAL: INTERACCIÓN CON EL USUARIO --------------------
------------------------------------------------------------------

-- Mostrar las estadisticas de un usuario en la pantalla

mostrarResultadosEstadisticas :: Estadisticas -> IO()
mostrarResultadosEstadisticas estadistica = do
    putStrLn("\n")
    putStrLn("\tBienvenido al corrector de notas inteligente.")
    putStrLn("\tEstos son los resultados de nuestra corrección automática.")

    putStrLn("\n\tBreve resumen:")
    putStrLn("\tLa nota media ha sido de " ++ (cogerNotaMedia estadistica))
    putStrLn("\tEl Numero Medio de Preguntas Respondidas es de " ++ (cogerMediaRespuestas estadistica))

    putStrLn("\n\t¿Cómo han ido tus alumnos?:")
    putStrLn("\t" ++ (cogerSuspensos estadistica) ++ " alumnos han sacado notable.")
    putStrLn("\t" ++ (cogerAprobados estadistica) ++ " alumnos han aprobado.")
    putStrLn("\t" ++ (cogerNotables estadistica) ++ " alumnos han sacado notable.")
    putStrLn("\t" ++ (cogerSobresalientes estadistica) ++ " alumnos lo han bordado con un sobresaliente!.")

    putStrLn("\n\tPreguntas:")
    putStrLn("\tPregunta con mejor resultado: " ++ (cogerPreguntaMejorResultado estadistica))
    putStrLn("\tPregunta con peor resultado: " ++ (cogerPreguntaPeorResultado estadistica))
    putStrLn("\tPregunta mas contestada: " ++ (cogerPreguntaMasContestada estadistica))
    putStrLn("\tPregunta menos contestada: " ++ (cogerPreguntaMenosContestada estadistica))

    -- putStrLn("\n\tAhora vamos :")
    -- putStrLn("\tFrecAbsRespuestasCorrectas: " ++ (cogerFrecAbsRespuestasCorrectas estadistica))
    -- putStrLn("\tFrecRelRespuestasCorrectas: " ++ (cogerFrecRelRespuestasCorrectas estadistica))
    -- putStrLn("\tFrecAbsRespuestasErroneas: " ++ (cogerFrecAbsRespuestasErroneas estadistica))
    -- putStrLn("\tFrecRelRespuestasErroneas: " ++ (cogerFrecRelRespuestasErroneas estadistica))
    -- putStrLn("\tFrecAbsRespuestasBlancos: " ++ (cogerFrecAbsRespuestasBlancos estadistica))
    -- putStrLn("\tFrecRelRespuestasBlancos: " ++ (cogerFrecRelRespuestasBlancos estadistica))

    putStrLn("\n\n\tTienes que subscribirte a la versión de pago para ver el resto de estadísticas :).\n\t[Comprar versión de pago...]")

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
    puntuacionSobre10 :: Float,
    tipoRespuesta :: [Int] -- Para cada pregunta, 0= No respondida | 1 = Acertada | 2 = Fallada
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
Ahora tenemos que hacer unas funciones que sean capaces de rellenas los campos
de correctas, erroneas y vacias. ¿Por qué necesitamos estos 3 campos?

Porque con la estructura de mi programa, es la manera más facil para luego
poder obtener las estadísticas. La idea de las siguientes funciones es la siguiente:

Pasas un test y un modelo de examen. Entonces estos atributos van a ser listas,
que a 1 indican si la pregunta fue (acertada, fallada o vacia). La peculiaridad
de estas listas es que el orden en el que van a ser devueltas NO es el orden del
modelo de examen, sino, el orden en el que nosotros tenemos guardadas las preguntas.
Esto es así para que luego a la hora de hacer las estadísticas sean muy cómodas.

Notese que en la función anteriormente descrita "cogerPreguntasDelModelo",
devolvíamos una lista de preguntas ordenadas según el modelo. Esto es así, a drede,
ya que queríamos poder comparar dichas preguntas con las del usuario. En cambio,
en estas próximas funciones eso NO lo queremos. Lo que queremos es devolverlo
en el orden en el que tenemos nuestras preguntas (por lo que te he dicho que
luego a la hora de hacer las estadísticas será mucho más cómodo).
--}

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
Getters - Métodos para coger atributos de corrección.
--}

cogerPuntuacionTotal :: Correccion -> Float
cogerPuntuacionTotal (Correccion _ puntuacion _) = puntuacion

cogerPuntuacionSobre10 :: Correccion -> Float
cogerPuntuacionSobre10 (Correccion _ _ puntuacion10) = puntuacion10

estaSupenso :: Correccion -> Bool
estaSupenso correccion = (cogerPuntuacionSobre10 correccion) < 5.0

estaAprobado :: Correccion -> Bool
estaAprobado correccion
    | puntuacion >= 5.0 && puntuacion < 7.0     = True
    | otherwise                                 = False
    where puntuacion = (cogerPuntuacionSobre10 correccion)

estaNotable :: Correccion -> Bool
estaNotable correccion
    | puntuacion >= 7.0 && puntuacion < 9.0     = True
    | otherwise                                 = False
    where puntuacion = (cogerPuntuacionSobre10 correccion)

estaSobresaliente :: Correccion -> Bool
estaSobresaliente correccion
    | puntuacion >= 9.0                         = True
    | otherwise                                 = False
    where puntuacion = (cogerPuntuacionSobre10 correccion)

{--
Funciones auxiliares para las estadísticas:
1. calcularPuntuacionMedia.
    Calcula las puntaciones media de toda la lista de correcciones.
    Para ello, primero llama a "sumarPuntuaciones", y luego divide
    la suma total entre el número de respuestas al test (esto lo podemos
    sacar cogiendo la longitud de correccion)

--}

calcularPuntuacionMedia :: [Correccion] -> Float
calcularPuntuacionMedia correccion =
    (sumarPuntuaciones correccion) / fromIntegral( length(correccion) )

-- Función auxiliar para calcularPuntuacionMedia
sumarPuntuaciones :: [Correccion] -> Float
sumarPuntuaciones [] = 0.0
sumarPuntuaciones (x:xs) =
    (cogerPuntuacionSobre10 x) + sumarPuntuaciones xs

calcularSuspensos :: [Correccion] -> Int
calcularSuspensos [] = 0
calcularSuspensos (x:xs)
    | suspenso == True     = 1 + (calcularSuspensos xs)
    | otherwise            = 0 + (calcularSuspensos xs)
    where suspenso = estaSupenso x

calcularAprobados :: [Correccion] -> Int
calcularAprobados [] = 0
calcularAprobados (x:xs)
    | aprobado == True     = 1 + (calcularAprobados xs)
    | otherwise            = 0 + (calcularAprobados xs)
    where aprobado = estaAprobado x

calcularNotables :: [Correccion] -> Int
calcularNotables [] = 0
calcularNotables (x:xs)
    | notable == True       = 1 + (calcularNotables xs)
    | otherwise             = 0 + (calcularNotables xs)
    where notable = estaNotable x

calcularSobresalientes :: [Correccion] -> Int
calcularSobresalientes [] = 0
calcularSobresalientes (x:xs)
    | sobresaliente == True   = 1 + (calcularSobresalientes xs)
    | otherwise               = 0 + (calcularSobresalientes xs)
    where sobresaliente = estaSobresaliente x

{--
Estadísticas
pregunta mas veces (respectivamente menos veces) dejada en blanco.
--}

data Estadisticas = Estadisticas {
    puntuacionMedia :: Float,
    numeroMedioPreguntasRespondidas :: Int,

    numeroSuspensos :: Int, -- nota < 5
    numeroAprobados :: Int, -- 5 <= nota < 7
    numeroNotables  :: Int, -- 7 <= nota < 9
    numeroSobresalientes :: Int, -- 9 <= nota

    frecAbsRespuestasCorrectas :: [Float],
    frecRelRespuestasCorrectas :: [Float],
    frecAbsRespuestasErroneas :: [Float],
    frecRelRespuestasErroneas :: [Float],
    frecAbsRespuestasBlancos :: [Float],
    frecRelRespuestasBlancos :: [Float],

    preguntaMejorResultado :: Int,
    preguntaPeorResultado :: Int,

    preguntaMasContestada :: Int,
    preguntaMenosContestada :: Int

} deriving (Show)

estadisticas :: Test -> [RespuestaEstudiante] -> Estadisticas
estadisticas test respuestas =
    (Estadisticas
        (calcularPuntuacionMedia correcciones)
        0

        (calcularSuspensos correcciones)
        (calcularAprobados correcciones)
        (calcularNotables correcciones)
        (calcularSobresalientes correcciones)

        [0.0, 0.0, 0.0, 0.0]
        [0.0, 0.0, 0.0, 0.0]
        [0.0, 0.0, 0.0, 0.0]
        [0.0, 0.0, 0.0, 0.0]
        [0.0, 0.0, 0.0, 0.0]
        [0.0, 0.0, 0.0, 0.0]

        0
        0

        0
        0
    )
    where correcciones = (corrige_todos test respuestas)


{--
Métodos para coger cada uno de los valores de la estadística
y devolver una cadena.

Este método es utilizado en la función que muestra en pantalla las
estadísticas de un usuario.
--}

cogerNotaMedia :: Estadisticas -> String
cogerNotaMedia (Estadisticas nota _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = show nota

cogerMediaRespuestas :: Estadisticas -> String
cogerMediaRespuestas (Estadisticas _ respuestas _ _ _ _ _ _ _ _ _ _ _ _ _ _) = show respuestas

cogerSuspensos :: Estadisticas -> String
cogerSuspensos (Estadisticas _ _ suspensos _ _ _ _ _ _ _ _ _ _ _ _ _) = show suspensos

cogerAprobados :: Estadisticas -> String
cogerAprobados (Estadisticas _ _ _ aprobados _ _ _ _ _ _ _ _ _ _ _ _) = show aprobados

cogerNotables :: Estadisticas -> String
cogerNotables (Estadisticas _ _ _ _ notables _ _ _ _ _ _ _ _ _ _ _) = show notables

cogerSobresalientes :: Estadisticas -> String
cogerSobresalientes (Estadisticas _ _ _ _ _ sobresalientes _ _ _ _ _ _ _ _ _ _) = show sobresalientes

cogerFrecAbsRespuestasCorrectas :: Estadisticas -> String
cogerFrecAbsRespuestasCorrectas (Estadisticas _ _ _ _ _ _ frec _ _ _ _ _ _ _ _ _) = show frec

cogerFrecRelRespuestasCorrectas :: Estadisticas -> String
cogerFrecRelRespuestasCorrectas (Estadisticas _ _ _ _ _ _ _ frec _ _ _ _ _ _ _ _) = show frec

cogerFrecAbsRespuestasErroneas :: Estadisticas -> String
cogerFrecAbsRespuestasErroneas (Estadisticas _ _ _ _ _ _ _ _ frec _ _ _ _ _ _ _) = show frec

cogerFrecRelRespuestasErroneas :: Estadisticas -> String
cogerFrecRelRespuestasErroneas (Estadisticas _ _ _ _ _ _ _ _ _ frec _ _ _ _ _ _) = show frec

cogerFrecAbsRespuestasBlancos :: Estadisticas -> String
cogerFrecAbsRespuestasBlancos (Estadisticas _ _ _ _ _ _ _ _ _ _ frec _ _ _ _ _) = show frec

cogerFrecRelRespuestasBlancos :: Estadisticas -> String
cogerFrecRelRespuestasBlancos (Estadisticas _ _ _ _ _ _ _ _ _ _ _ frec _ _ _ _) = show frec

cogerPreguntaMejorResultado :: Estadisticas -> String
cogerPreguntaMejorResultado (Estadisticas _ _ _ _ _ _ _ _ _ _ _ _ pregunta _ _ _) = show pregunta

cogerPreguntaPeorResultado :: Estadisticas -> String
cogerPreguntaPeorResultado (Estadisticas _ _ _ _ _ _ _ _ _ _ _ _ _ pregunta _ _) = show pregunta

cogerPreguntaMasContestada :: Estadisticas -> String
cogerPreguntaMasContestada (Estadisticas _ _ _ _ _ _ _ _ _ _ _ _ _ _ pregunta _) = show pregunta

cogerPreguntaMenosContestada :: Estadisticas -> String
cogerPreguntaMenosContestada (Estadisticas _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ pregunta) = show pregunta
