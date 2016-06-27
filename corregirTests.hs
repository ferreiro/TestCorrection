import Data.List
import Data.Char

------------------------------------------------------------------
-- PARTE OPCIONAL: INTERACCIÓN CON EL USUARIO --------------------
------------------------------------------------------------------


-- Pedir nombre del cliente y mostrar las estadisticas de un examen en la pantalla
-- Si quieres ejecutar el programa en tu ordenador y ver los resultados del test
-- simplemente ejecuta la función mostrar (_estadisticas es un dato de prueba
-- que he generado, puedes verlo más detenidamente en el siguiente bloque)


mostrar = mostrarResultadosEstadisticas _estadisticas

mostrarResultadosEstadisticas :: Estadisticas -> IO()
mostrarResultadosEstadisticas estadistica = do

    putStrLn("\nHola! Cómo te llamas?")
    nombre <- getLine

    putStrLn("\n")
    putStrLn("\tBienvenido " ++ nombre ++ " al corrector de notas inteligente.")
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

    putStrLn("\n\t¿Curioso por saber las frecuencias de las respuestas? ¡Ahí van! :")
    putStrLn("\tFrecAbsRespuestasCorrectas: " ++ (cogerFrecAbsRespuestasCorrectas estadistica))
    putStrLn("\tFrecRelRespuestasCorrectas: " ++ (cogerFrecRelRespuestasCorrectas estadistica))
    putStrLn("\tFrecAbsRespuestasErroneas: " ++ (cogerFrecAbsRespuestasErroneas estadistica))
    putStrLn("\tFrecRelRespuestasErroneas: " ++ (cogerFrecRelRespuestasErroneas estadistica))
    putStrLn("\tFrecAbsRespuestasBlancos: " ++ (cogerFrecAbsRespuestasBlancos estadistica))
    putStrLn("\tFrecRelRespuestasBlancos: " ++ (cogerFrecRelRespuestasBlancos estadistica))
    putStrLn("\n")


-----------------------------------------------
-- DATOS DE PRUEBA ----------------------------
------------------------------------------------


-- Estos son los datos de prueba que he generado.
-- En total tenemos 5 preguntas. 2 modelos diferentes de examenes. 1 Test.
-- y 3 respuestas de diferentes estudiantes.

-- Para saber qué significado tiene cada elemento en el tipo de dato,
-- puedes buscar el tipo de dato en el programa y ver sus parámetros.


preguntas = [ (Pregunta 1 2), (Pregunta 1 6), (Pregunta 1 2), (Pregunta 1 2), (Pregunta 1 2) ]
modelos = [ (Modelo [1, 2, 3, 4, 5]), (Modelo [5, 4, 3, 2, 1])]
test = (Test preguntas modelos)

respuestas = [
      (RespuestaTest "George" 1 [(Respuesta 2), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 0)])
    , (RespuestaTest "ABC-W" 1 [(Respuesta 0), (Respuesta 1), (Respuesta 0), (Respuesta 0), (Respuesta 0)])
    , (RespuestaTest "WWW-D" 1 [(Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 1), (Respuesta 0)]) ]

_correcciones = (corrige_todos test respuestas)
_estadisticas = estadisticas test respuestas

--------------------------------------------
-- ESTRUCTURAS DE DATOS --------------------
--------------------------------------------

{--
Cada TEST tiene una lista de Preguntas y otra lista de Modelos de examen.

- Una PREGUNTA tiene dos campos: el número de opciones disponibles y el índice
de la respuesta correcta.

- Un MODELO, es una permutación de las preguntas. Internamente he representado
cada permutación como una lista de enteros, donde cada valor de los elementos
nos dice el índice de la pregunta a la que hace referencia. La misión de esta lista es
reflejar las permutaciones posibles. Cada elemento de la lista hace referencia
a la lista "preguntas".

Por tanto, tener un modelo [1, 2, 3] significa, que las preguntas están ordenadas
y que por tanto coindicden con la lista de preguntas_examen.
Por el contrario, tener un modelo como [3, 2, 1], significa que la primera
pregunta del modelo, es la tercera del examen y a lo mismo con el resto.
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
    permutacion :: [Int] -- Orden de las preguntas del test (permutaciones) | Cada elemento indica el indice de la pregunta en "preguntas" de Test
} deriving (Show)

-- Metodos auxiliares: getters
-- Devuelve el número de Preguntas que tiene un Test
cogerNumeroPreguntas :: Test -> Int
cogerNumeroPreguntas (Test preguntas modelos) = length(preguntas)

--Devuelve las permutaciones de preguntas que tiene un modelo
cogerPermutaciones :: Modelo -> [Int]
cogerPermutaciones (Modelo permutacion) = permutacion

{--
Ahora vamos a definir un tipo de datos para las respuestas.
Cada respuesta tiene un identificador del estudiante (En mi caso, voy a utilizar
el DNI/Nombre de cada estudiante que estará representado como una cadena).
Además de ello, necesitamos tener una lista de respuestas introducidas por el
estudianto y el número del modelo de examen (para poder corregir las preguntas
correctamente).

Seguiré la siguiente convención para los valores de las respuestas:
- 0 = el estudiante no ha contestado a la pregunta
- >0 = la respuesta sí ha sido contestada
--}

data RespuestaTest = RespuestaTest {
    identificador :: String,
    modelo :: Int,
    respuestas_estudiante :: [Respuesta]
} deriving (Show)

data Respuesta = Respuesta {
    valor :: Int
}  deriving (Show)

{--
Para poder corregir los tests de nuestros estudiantes, necesitamos tener una
función que dado: un test, las respuestas del estudiante y el modelo de examen
devuelva la corrección.

Una corrección está compuesta por:
- identificadorAlumno = Cadena con el nombre o DNI del estudiante
- puntuaciónTotal = Suma total de cada una de las preguntas
- puntuacionSobre10 = Nota obtenida por el estudiante
- respuestasOrdenadas = Es una lista ORDENADA en función de las preguntas
  del test cuyos valores pueden ser:
  (-1) = Pregunta fallada por el estudiante
     0 = Pregunta no contestada
     1 = Pregunta acertada por el estudiante

  [IMPORTANTE] Esta lista NO está ordenada según el modelo del examen.
  Sino que está ordenada en función de la lista de preguntas que tiene un test.
  Esto es una parte muy importante de mi programa, ya que gracias a ello,
  luego puedo obtener las estadísticas de una manera bastante cómoda.

  ¿Por qué opté por ordenarlo según el test y no el modelo? Porque en las
  estadísticas queremos saber las frecuencias para cada pregunta. Por tanto,
  si tenemos varios modelos, sería bastante lioso tener que estar continuamente
  convirtiendo cada respuesta (aunque esto depende de cómo organices tu programa
  y las estructuras de datos). En mi caso, de esta forma me ha funcionado y
  estoy bastante contento.
--}

data Correccion = Correccion {
    identificadorAlumno :: String,
    puntuacionTotal :: Float,
    puntuacionSobre10 :: Float,
    respuestasOrdenadas :: [Int] -- Para cada pregunta, 0= No respondida | 1 = Acertada | 2 = Fallada
}  deriving (Show)

-- Corrige todos los examenes de varios alumnos.
-- Recibe el test y las repuestas de los alumnos y
-- devuelve una lista de correcciones para cada alumno.
corrige_todos :: Test -> [RespuestaTest] -> [Correccion]
corrige_todos _ [] = []
corrige_todos test (x: xs) =
        corrige test x : corrige_todos test xs

-- Función corrige un examen de un estudiante
corrige :: Test -> RespuestaTest -> Correccion
corrige (Test preguntas modelos) (RespuestaTest identificador indiceModelo respuestas) =
        (Correccion
            identificador
            (notaEstudiante (cogerPreguntasDelModelo preguntas (modelos !! (indiceModelo-1))) respuestas)
            (puntuacion10 (length preguntas) (notaEstudiante (cogerPreguntasDelModelo preguntas (modelos !! (indiceModelo-1))) respuestas))
            (rellenarTipoRespuesta preguntas (generarRespuestasOrdenadas respuestas (modelos !! (indiceModelo-1)))) -- Convertimos un modelo desordenado en una lista ordenada
        )

{--
Métodos auxiliares para obtener atributos
del tipo de Dato Corrección
--}

cogerTipoRespuesta :: Correccion -> [Int]
cogerTipoRespuesta (Correccion _ _ _ respuestasOrdenadas) =  respuestasOrdenadas

cogerNotaObtenida :: Correccion -> Float
cogerNotaObtenida (Correccion _ _ puntuacion10 _) =  puntuacion10

cogerPuntuacionTotal :: Correccion -> Float
cogerPuntuacionTotal (Correccion _ puntuacion _ _) = puntuacion

cogerPuntuacionSobre10 :: Correccion -> Float
cogerPuntuacionSobre10 (Correccion _ _ puntuacion10 _) = puntuacion10

{--
A continuación vamos a obtener una lista de preguntas que seguirá el
orden de las permutaciones que tiene el modelo. Con esta lista de preguntas,
podemos corregir las respuestas dadas por el estudiante.

El modelo cabe recordar es una lista de enteros que representan las permutaciones
de las preguntas del test. Por tanto, en esta parte del programa, tendremos
que hacer la conversión para obtener la lista de preguntas.
--}

-- Función Wrapper que devolverá una lista de preguntas
-- ordenadas según la permutación del modelo
cogerPreguntasDelModelo :: [Pregunta] -> Modelo -> [Pregunta]
cogerPreguntasDelModelo preguntasDelTest modelo =
   cogerPreguntasDadoOrden preguntasDelTest (cogerPermutaciones modelo)

-- Esta función va componiendo la lista de preguntas (que como te he
-- comentado sigue el orden de las permutaciones del modelo)
cogerPreguntasDadoOrden :: [Pregunta] -> [Int] -> [Pregunta]
cogerPreguntasDadoOrden [] _ = []
cogerPreguntasDadoOrden _ [] = []
cogerPreguntasDadoOrden (preguntas) (indice:restoIndices) =
    (preguntas !! (indice - 1)) : (cogerPreguntasDadoOrden preguntas restoIndices)-- coger la pregunta

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
Ahora tenemos que hacer unas funciones que sean capaces de rellenas el campo
de respuestasOrdenadas. ¿Por qué necesitamos este campo?

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
También necesitamos una función que dado un conjunto de respuestas y un modelo,
ordene dichas respuestas (que vendría en el orden que ha impuesto el modelo)
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




calcularFrecuenciaRelativa :: Int -> [Float] -> [Float]
calcularFrecuenciaRelativa tamanoMuestra frecuenciasAbsoluta =
    map (\x -> x / fromIntegral(tamanoMuestra)) frecuenciasAbsoluta



-- Frecuencia Absoluta de respuestas vacias
calcularFrecuenciAbsolutaVacias :: [Correccion] -> [Float]
calcularFrecuenciAbsolutaVacias correcciones =
    foldl1 (zipWith (+)) (map dejarCorreccionVacias (map cogerTipoRespuesta correcciones))

dejarCorreccionVacias :: [Int] -> [Float]
dejarCorreccionVacias x = map quedarmeConVacias x

quedarmeConVacias :: Int -> Float
quedarmeConVacias x
    | x == 0    = 1.0
    | x == 1    = 0.0
    | otherwise = 0.0


-- Frecuencia Absoluta de  respuestas correctas
calcularFrecuenciAbsolutaCorrectas :: [Correccion] -> [Float]
calcularFrecuenciAbsolutaCorrectas correcciones =
    foldl1 (zipWith (+)) (map dejarCorreccionCorrectas (map cogerTipoRespuesta correcciones))

dejarCorreccionCorrectas :: [Int] -> [Float]
dejarCorreccionCorrectas x = map quedarmeConCorrectas x

quedarmeConCorrectas :: Int -> Float
quedarmeConCorrectas x
    | x == 0    = 0.0
    | x == 1    = 1.0
    | otherwise = 0.0

-- Frecuencia Absoluta de  respuestas erroneas
calcularFrecuenciAbsolutaErroneas :: [Correccion] -> [Float]
calcularFrecuenciAbsolutaErroneas correcciones =
    foldl1 (zipWith (+)) (map dejarCorreccionErroneas (map cogerTipoRespuesta correcciones))

dejarCorreccionErroneas :: [Int] -> [Float]
dejarCorreccionErroneas x = map quedarmeConErroneas x

quedarmeConErroneas :: Int -> Float
quedarmeConErroneas x
    | x == 0    = 0.0
    | x == 1    = 0.0
    | otherwise = 1.0







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
    -- Esto se conoce como sumar une lista de listas
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

estadisticas :: Test -> [RespuestaTest] -> Estadisticas
estadisticas test respuestas =
    (Estadisticas
        (calcularPuntuacionMedia correcciones)
        (calcularNumeroMedioPreguntasRespondidas correcciones (cogerNumeroPreguntas test))

        (calcularSuspensos correcciones)
        (calcularAprobados correcciones)
        (calcularNotables correcciones)
        (calcularSobresalientes correcciones)

        -- Primero calculamos las frecuencias absolutas-
        -- La frecuencia relativa es el cociente entre la frecuencia absoluta
        -- y el tamaño de la muestra (N) en nuestro caso la longitud de respuestas
        frecuenciaAbsolutaCorrectas
        (calcularFrecuenciaRelativa tamanoMuestra frecuenciaAbsolutaCorrectas)

        frecuenciaAbsolutaErroneas
        (calcularFrecuenciaRelativa tamanoMuestra frecuenciaAbsolutaCorrectas)

        frecuenciaAbsolutaVacias
        (calcularFrecuenciaRelativa tamanoMuestra frecuenciaAbsolutaCorrectas)

        -- Mejores resultados y mas blancas
        (conseguirPreguntaMejorResultado correcciones)
        (conseguirPeorPeorResultado correcciones)

        (conseguirPreguntaMasBlanca correcciones)
        (conseguirPreguntaMenosBlanca correcciones)
    )
    where correcciones = (corrige_todos test respuestas)
          tamanoMuestra = length(respuestas)
          frecuenciaAbsolutaCorrectas = (calcularFrecuenciAbsolutaCorrectas correcciones)
          frecuenciaAbsolutaErroneas = (calcularFrecuenciAbsolutaErroneas correcciones)
          frecuenciaAbsolutaVacias = (calcularFrecuenciAbsolutaVacias correcciones)


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
