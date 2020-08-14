module Oraculo (Oraculo(..),
                Opciones,
                crearOraculo,
                prediccion, 
                pregunta,
                opciones,
                respuesta,
                ) where

  import qualified Data.Map as Map

-- Definición de datos
  type Opciones = Map.Map String Oraculo
  data Oraculo = Pregunta String Opciones | Prediccion String
    deriving (Show, Read)





-- Crear
  crearOraculo :: String -> Oraculo
  crearOraculo txt = Prediccion txt

-- Acceder
  prediccion :: Oraculo -> String
  prediccion (Prediccion txt) = txt
  prediccion _ = error "El oráculo no es una predicción"

  pregunta :: Oraculo -> String
  pregunta (Pregunta txt _) = txt
  pregunta _ = error "El oráculo no es una pregunta"

  opciones :: Oraculo -> Opciones
  opciones (Pregunta _ ops) = ops
  opciones _ = error "El oráculo no es una pregunta"

  respuesta :: Oraculo -> String -> Oraculo
  respuesta (Pregunta _ ops) resp = ops Map.! resp
  respuesta _ _ = error "El oráculo no es una pregunta"
