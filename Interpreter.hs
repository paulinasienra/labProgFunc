-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DEL INTÉRPRETE DEL LENGUAJE DE MÁQUINA

module Interpreter where

import MachineLang
import Data.Char (chr, ord)

type Conf = (Stack,Env)

type Env = [(Var,Integer)]
type Stack = [Integer]

-- Implementar
interp :: Code -> Code -> Conf -> IO Conf
interp _ [SKIP] conf = return conf -- caso base, termino el codigo
interp pc (SKIP:xs) conf = interp (SKIP:pc) xs conf
interp pc (ADD:xs) (n:f:ys,amb) = interp (ADD:pc) xs ((n + f):ys,amb)
interp pc (SUB:xs) (n:f:ys,amb) = interp (SUB:pc) xs ((n - f):ys,amb)
interp pc (MUL:xs) (n:f:ys,amb) = interp (MUL:pc) xs ((n * f):ys,amb)
interp pc (DIV:xs) (n:f:ys,amb) = interp (DIV:pc) xs (div n f:ys,amb)
interp pc (MOD:xs) (n:f:ys,amb) = interp (MOD:pc) xs (mod n f:ys,amb)
interp pc (NEG:xs) (n:ys,amb) = interp (NEG:pc) xs (-n:ys,amb)
interp pc (CMP:xs) (n:f:ys,amb) | n == f = interp (CMP:pc) xs (0:ys,amb)
                                | n < f = interp (CMP:pc) xs (1:ys,amb)
                                | otherwise = interp (CMP:pc) xs (-1:ys,amb)
interp pc (PUSH n:xs) (ys,amb) = interp (PUSH n:pc) xs (n:ys,amb)
interp pc (JMPZ n:xs) (m:ys,amb) | m == 0 && n > 0 = interp (reverse (take n xs) ++ JMPZ n:pc) (drop n xs) (ys,amb)
                                 | m == 0 = interp (drop n pc) (reverse (take n pc)++ JMPZ n:xs) (ys,amb)
                                 | otherwise = interp (JMPZ n:pc) xs (ys,amb)
interp pc (JUMP n:xs) (ys,amb) | n > 0 = interp (reverse (take n xs) ++ JUMP n:pc) (drop n xs) (ys,amb) -- Jump puede ser negativo
                                | otherwise  = interp (drop n pc) (reverse (take n pc)++ JUMP n:xs) (ys,amb)
interp pc (WRITE: xs) (n:ys,amb) = do putStr [chr (fromIntegral  n)];
                                      interp (WRITE:pc) xs (ys,amb)
interp pc (READ:xs) (ys,amb) = do line <- getChar;
                                  interp (READ:pc) xs (toInteger(ord line):ys,amb)
interp pc (STORE s:xs) (n:ys,amb) = interp (STORE s:pc) xs (ys,updVar (s,n) amb)
interp pc (LOAD s:xs) (ys,amb) = interp (LOAD s:pc) xs (buscarValor s amb:ys,amb)


buscarValor :: Var -> Env -> Integer
buscarValor a (x:amb) | a ==fst x = snd x
                      | otherwise = buscarValor a amb


updVar :: (Var,Integer) -> Env -> Env
updVar a [] = [a]
updVar a (x:amb) | fst a ==fst x = updVar a amb
                                | otherwise = x:updVar a amb