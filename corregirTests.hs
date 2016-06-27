import Data.List
import Data.Char

---------------------------------------
-- EJEMPLOS ---------------------------
---------------------------------------

preguntas = [
      (Pregunta 1 2)
    , (Pregunta 1 6)
    , (Pregunta 1 2)
    , (Pregunta 1 2)
    , (Pregunta 1 2) ]

modelos = [
      (Modelo [1, 2, 3, 4, 5])
    , (Modelo [5, 4, 3, 2, 1])]

test = (Test preguntas modelos)

respuestas = [
    (RespuestaEstudiante "George" 1 [(Respuesta 2), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 0)])
    , (RespuestaEstudiante "ABC-W" 1 [(Respuesta 0), (Respuesta 1), (Respuesta 0), (Respuesta 0), (Respuesta 0)]) ]
    -- , (RespuestaEstudiante "WWW-D" 1 [(Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 0)])
    -- , (RespuestaEstudiante "DSC-W" 1 [(Respuesta 0), (Respuesta 1), (Respuesta 0), (Respuesta 0), (Respuesta 0)])
    -- , (RespuestaEstudiante "414992032-W" 1 [(Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1)])]

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
    putStrLn("\tEl " ++ (cogerMediaRespuestas estadistica) ++ "% de las preguntas han sido contestadas")

    putStrLn("\n\t¿Cómo han ido tus alumnos?:")
    putStrLn("\t" ++ (cogerSuspensos estadistica) ++ " alumnos han suspendido.")
    putStrLn("\t" ++ (cogerAprobados estadistica) ++ " alumnos han aprobado.")
    putStrLn("\t" ++ (cogerNotables estadistica) ++ " alumnos han sacado notable.")
    putStrLn("\t" ++ (cogerSobresalientes estadistica) ++ " alumnos lo han bordado con un sobresaliente!.")

    putStrLn("\n\tPreguntas:")
    putStrLn("\tPregunta con mejor resultado: " ++ (cogerPreguntaMejorResultado estadistica))
    putStrLn("\tPregunta con peor resultado: " ++ (cogerPreguntaPeorResultado estadistica))
    putStrLn("\tPregunta más veces dejada en blanco: " ++ (cogerPreguntaMasBlanca estadistica))
    putStrLn("\tPregunta menos veces dejada en blanco: " ++ (cogerPreguntaMenosBlanca estadistica))

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

cogerNumeroPreguntas :: Test -> Int
cogerNumeroPreguntas (Test preguntas modelos) = length(preguntas)

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
            (rellenarTipoRespuesta preguntas (generarRespuestasOrdenadas respuestas (modelos !! (indiceModelo-1)))) -- Convertimos un modelo desordenado en una lista ordenada
        )

{--
Getters - Métodos para coger atributos de corrección.
--}

cogerTipoRespuesta :: Correccion -> [Int]
cogerTipoRespuesta (Correccion _ _ _ tipoRespuesta) =  tipoRespuesta

cogerNotaObtenida :: Correccion -> Float
cogerNotaObtenida (Correccion _ _ puntuacion10 _) =  puntuacion10

cogerPuntuacionTotal :: Correccion -> Float
cogerPuntuacionTotal (Correccion _ puntuacion _ _) = puntuacion

cogerPuntuacionSobre10 :: Correccion -> Float
cogerPuntuacionSobre10 (Correccion _ _ puntuacion10 _) = puntuacion10

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

-- va componiendo una lista de preguntas siguiente el orden dado
cogerPreguntasDadoOrden :: [Pregunta] -> [Int] -> [Pregunta]
cogerPreguntasDadoOrden [] _ = []
cogerPreguntasDadoOrden _ [] = []
cogerPreguntasDadoOrden (preguntas) (indice:restoIndices) =
    (preguntas !! (indice - 1)) : (cogerPreguntasDadoOrden preguntas restoIndices)-- coger la pregunta

cogerOrdenPreguntas :: Modelo -> [Int]
cogerOrdenPreguntas (Modelo ordenPreguntas) = ordenPreguntas

{--
Ahora tenemos que hacer unas funciones que sean capaces de rellenas el campo
de tipoRespuesta. ¿Por qué necesitamos este campo?

Porque con la estructura de mi programa, es la manera más facil para luego
poder obtener las estadísticas. La idea es coger las respuestas dadas para un modelo
y ordenar dichas respuestas según las preguntas del test y NO el modelo
(esto hace que luego podamos sacar estadísticas para cada pregunta, porque sabemos
que todas las correcciones están ordenadas de la misma manera y no según el tipo de modelo)

Dado un conjunto de respuestas ORDENADAS según nuestro test (y no según nuestro modelo de examen),
devuelve una lista de enteros con la respuestas corregidas donde cada elemento puede tener estos valores:

-1 = Pregunta fallada
0 = Pregunta no contestada
1 = Pregunta acertada

Parametros entrada:
- Lista de preguntas (ordenadas según el test y NO según el modelo)
- Lista de respuestas (ordenadas según el test y NO según el modelo)

Devuelve:
- Una lista con los elementos corregidos
--}

rellenarTipoRespuesta :: [Pregunta] -> [Respuesta] -> [Int]
rellenarTipoRespuesta [] _ = []
rellenarTipoRespuesta _ [] = []
rellenarTipoRespuesta (p: preguntas) (r: respuestas) =
    (respuestaCorrecta p r) : (rellenarTipoRespuesta preguntas respuestas)

respuestaCorrecta :: Pregunta -> Respuesta -> Int
respuestaCorrecta (Pregunta respuestaCorrecta opciones) (Respuesta respuestaUsuario)
    | respuestaUsuario == 0                   = 0 -- Respuesta no contestada
    | respuestaCorrecta == respuestaUsuario   = 1 -- respuesta acertada
    | otherwise                               = (-1) -- respuesta fallada

{--
La misión de esta función es dado un conjunto de respuestas y un modelo,
entonces ordena dichas respuestas (que vendría en el orden que ha impuesto el modelo)
y las ordena según el orden en el que hemos declarado nosotros las preguntas
de nuestro test. Es decir, es una función conversora partiendo de respuestas
en el orden del modelo y devolviendo respuestas en el orden de nuestro test.
--}

generarRespuestasOrdenadas :: [Respuesta] -> Modelo -> [Respuesta]
generarRespuestasOrdenadas respuestas (Modelo ordenRespuestasEnModelo) =
    ordenarRespuestas
        [1, 2..length(respuestas)] -- Así es como queremos ordenar nuestras respuestas
        ordenRespuestasEnModelo -- El modelo puede tener [3, 2, 1] (JUsto al reves... entonces queremos darle la vuelta)
        respuestas

ordenarRespuestas :: [Int] -> [Int] -> [Respuesta] -> [Respuesta]
ordenarRespuestas [] _ _ = []
ordenarRespuestas _ [] _ = []
ordenarRespuestas _ _ [] = []
ordenarRespuestas (x:xs) ordenRespuestasModelo respuestas =
   (respuestas !! (indiceRespuestaEnModelo x ordenRespuestasModelo)) : ordenarRespuestas xs ordenRespuestasModelo respuestas

-- Encuentra en dónde está la respuesta según el orden que tiene el modelo
indiceRespuestaEnModelo :: Int -> [Int] -> Int
indiceRespuestaEnModelo x ordenModelo = maybeToInt(findIndex (==x) ordenModelo)

maybeToInt :: Maybe Int -> Int
maybeToInt a = digitToInt ((show a) !! 5)

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
Funciones auxiliares para las estadísticas:
1. tuacionMedia.
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

-- Calcula el numero medio de preguntas respondidas
-- Por tanto, sería calcular el número de respuestas en blanco para
-- cada persona y luego restarlo al número de preguntas en total.

calcularNumeroMedioPreguntasRespondidas :: [Correccion] -> Int -> Float
calcularNumeroMedioPreguntasRespondidas correcciones numeroPreguntas =
   -- Regla de tress. Formula: (preguntasRespondidas*100) / preguntas totales
   (fromIntegral (sumarPreguntasRespondidas correcciones) * 100) / fromIntegral (numeroPreguntas * length(correcciones))

sumarPreguntasRespondidas :: [Correccion] -> Int
sumarPreguntasRespondidas [] = 0
sumarPreguntasRespondidas (x:xs) =
   (calcularRespuestasContestadas (cogerTipoRespuesta x)) + (sumarPreguntasRespondidas xs)

calcularRespuestasContestadas :: [Int] -> Int
calcularRespuestasContestadas respuestas = length(filter (>0) respuestas) -- coge aquellas preguntas que sí han sido contestada (1==correcta|2==fallo)

calcularSuspensos :: [Correccion] -> Int
calcularSuspensos x = length((filter (<5) (map cogerNotaObtenida x) ))

calcularAprobados :: [Correccion] -> Int
calcularAprobados x = length(filter (>=5) ((filter (<7)(map cogerNotaObtenida x))))

calcularNotables :: [Correccion] -> Int
calcularNotables x = length(filter (>=7) ((filter (<9)(map cogerNotaObtenida x))))

calcularSobresalientes :: [Correccion] -> Int
calcularSobresalientes x = length(filter (>=9) (map cogerNotaObtenida x))



conseguirPreguntaMejorResultado :: [Correccion] -> Int
conseguirPreguntaMejorResultado correcciones =
    1 + maybeToInt(findIndex (==maximum(sumarTodosLosResultados correcciones)) (sumarTodosLosResultados correcciones)) -- coges el valor con mayor número y luego buscas su indice en la lista y lo devuelves

conseguirPeorPeorResultado :: [Correccion] -> Int
conseguirPeorPeorResultado correcciones =
    1 + maybeToInt(findIndex (==minimum(sumarTodosLosResultados correcciones)) (sumarTodosLosResultados correcciones)) -- coges el valor con mayor número y luego buscas su indice en la lista y lo devuelves

conseguirPreguntaMasBlanca :: [Correccion] -> Int
conseguirPreguntaMasBlanca correcciones =
    1 + maybeToInt(findIndex (==maximum(sumarRespuestasEnBlanco correcciones)) (sumarRespuestasEnBlanco correcciones)) -- coges el valor con mayor número y luego buscas su indice en la lista y lo devuelves

conseguirPreguntaMenosBlanca :: [Correccion] -> Int
conseguirPreguntaMenosBlanca correcciones =
    1 + maybeToInt(findIndex (==minimum(sumarRespuestasEnBlanco correcciones)) (sumarRespuestasEnBlanco correcciones)) -- coges el valor con mayor número y luego buscas su indice en la lista y lo devuelves

{--
Para calcular las preguntas más populares y menos, voy a hacer lo siguiente
Sumar todas las respuestas en una lista y en base a eso, quedarme con los
valores con mayor o menor valor
--}
sumarTodosLosResultados :: [Correccion] -> [Int]
sumarTodosLosResultados correcciones =
  foldl1 (zipWith (+)) (map cogerTipoRespuesta correcciones)  -- Sumar lista de listas en una

sumarRespuestasEnBlanco :: [Correccion] -> [Int]
sumarRespuestasEnBlanco correcciones =
  foldl1 (zipWith (+)) (map dejarRespuestasEnBlanco (map cogerTipoRespuesta correcciones))  -- Sumar lista de listas en una

-- Quitar aquellas respuestas que son -1, 1.
-- Sabemos que las respuestas no contestadas son aquellas que tienen un 0.
-- Por tanto, lo que voy a hacer es para cada lista, quitar los aciertos y fallos
-- poniendolo a 0, y poner a 1 la pregunta que no ha sido contestada
dejarRespuestasEnBlanco :: [Int] -> [Int]
dejarRespuestasEnBlanco x = map quedarmeConBlanco x

quedarmeConBlanco :: Int -> Int
quedarmeConBlanco x
    | x == 0    = 1
    | x == 1    = 0
    | otherwise = 0

-- zipWith (\x y -> 2*x + y) [1..4] [5..8]

{--
Estadísticas
pregunta mas veces (respectivamente menos veces) dejada en blanco.
--}

data Estadisticas = Estadisticas {
    puntuacionMedia :: Float,
    numeroMedioPreguntasRespondidas :: Float,

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

    preguntaMasBlanca :: Int,
    preguntaMenosBlanca :: Int

} deriving (Show)

estadisticas :: Test -> [RespuestaEstudiante] -> Estadisticas
estadisticas test respuestas =
    (Estadisticas
        (calcularPuntuacionMedia correcciones)
        (calcularNumeroMedioPreguntasRespondidas correcciones (cogerNumeroPreguntas test))

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

        (conseguirPreguntaMejorResultado correcciones)
        (conseguirPeorPeorResultado correcciones)

        (conseguirPreguntaMasBlanca correcciones)
        (conseguirPreguntaMenosBlanca correcciones)
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

cogerPreguntaMasBlanca :: Estadisticas -> String
cogerPreguntaMasBlanca (Estadisticas _ _ _ _ _ _ _ _ _ _ _ _ _ _ pregunta _) = show pregunta

cogerPreguntaMenosBlanca :: Estadisticas -> String
cogerPreguntaMenosBlanca (Estadisticas _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ pregunta) = show pregunta
