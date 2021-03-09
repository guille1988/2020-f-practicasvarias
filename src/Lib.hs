module Lib where
import Text.Show.Functions
import Data.List

laVerdad = True

-- FINAL 14/12/19

type ListaDeFunciones = [Int -> Int]
type Numero = Int
type ListaDeNumeros = [Int]

aplicarYfiltrar :: Numero -> Numero -> ListaDeFunciones -> ListaDeNumeros
aplicarYfiltrar numero numeroAevaluar listaDeFunciones = filtrarLosMayoresA (aplicarFunciones numero listaDeFunciones) numeroAevaluar

filtrarLosMayoresA :: ListaDeNumeros -> Numero -> ListaDeNumeros
filtrarLosMayoresA listaDeNumeros numeroAevaluar = filter (> numeroAevaluar) listaDeNumeros

aplicarFunciones :: Numero -> ListaDeFunciones -> ListaDeNumeros
aplicarFunciones numero listaDeFunciones = map (\f -> f numero) listaDeFunciones 

-- FINAL DICIEMBRE 2020

f1 h = any (>15). map h   

fi h = (>0). length. filter (>15) . map h 

--1. Ambas funciones llegan al mismo resultado pero de diferente forma, no hacen lo mismo las funciones que la componen. Especie de equifinalidad.
--2. La primera funcion llega a un resultado si tiene una lista infinita ya que gracias a Lazy Evaluation si detecta aunque sea un numero mayor a 15, deja de evaluar el resto
-- Si le pones la lista [-99,-100..] y de la funcion h es (+1), obviamente nunca va a llegar a un resultado, porque va a seguir evaluando infinitamente. Depende mucho de h tmb.
-- La segunda función no llega nunca a un resultado ya que ese filter, necesita si o si toda la lista. 
--3. Las 2 son de orden superior, ya que las 2 son funciones que reciben o entregan funciones.