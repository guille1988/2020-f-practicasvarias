module Lib where
import Text.Show.Functions
import Data.List

laVerdad = True

funcion x y lista = (filter (> x) . map (\ f -> f y)) lista

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

-- FINAL 23/02/2019
