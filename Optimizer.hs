-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE OPTIMIZACIÓN
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Optimizer where

import Syntax


-- Implementar
optimize :: Program -> Program
optimize = undefined

-- COSAS PARA ARREGLAR
-- Todos los úlimos casos recursivos quedan en loop si una de las sub-expresiones no se reduce a un NatLit


--Se supone que primero se optimiza la subexpresión izquierda
--luego la derecha, y después la raíz
exprOpt :: Expr -> Expr
exprOpt (NatLit i) = NatLit i
exprOpt (CharLit c)  = CharLit c
exprOpt GetChar = GetChar 
exprOpt (Var v) = Var v

--OPERADORES LÓGICOS
exprOpt (Binary Or (NatLit 1) ex) = NatLit 1
exprOpt (Binary Or ex (NatLit 1)) = NatLit 1
exprOpt (Binary Or exp1 exp2) = exprOpt (Binary Or (exprOpt exp1) (exprOpt exp2))

exprOpt (Binary And (NatLit 0) ex) = NatLit 0
exprOpt (Binary And ex (NatLit 0)) = NatLit 0
exprOpt (Binary And exp1 exp2) = exprOpt (Binary And (exprOpt exp1) (exprOpt exp2))

exprOpt (Binary Equ (NatLit n1) (NatLit n2)) | n1 == n2 = NatLit 1
                                             | otherwise  = NatLit 0
exprOpt (Binary Equ exp1 exp2) = exprOpt (Binary Equ (exprOpt exp1) (exprOpt exp2))

exprOpt (Binary Less (NatLit n1) (NatLit n2)) | n1 < n2 = NatLit 1
                                              | otherwise = NatLit 0
exprOpt (Binary Less exp1 exp2) = exprOpt (Binary Less (exprOpt exp1) (exprOpt exp2))

exprOpt (Unary Not (NatLit x)) | x /= 0      = NatLit 0
                               | otherwise  = NatLit 1
exprOpt (Unary Not expr) = exprOpt (Unary Not (exprOpt expr))

--OPERADORES ARITMÉTICOS
exprOpt (Binary Plus (NatLit 0) ex) = exprOpt ex
exprOpt (Binary Plus ex (NatLit 0)) = exprOpt ex
exprOpt (Binary Plus (NatLit n1) (NatLit n2)) = NatLit (n1 + n2)
exprOpt (Binary Plus exp1 exp2) = exprOpt (Binary Plus (exprOpt exp1) (exprOpt exp2))

exprOpt (Binary Minus exp (NatLit 0)) = exprOpt exp
exprOpt (Binary Minus (NatLit n1) (NatLit n2)) = NatLit (n1 - n2)
exprOpt (Binary Minus exp1 exp2) = exprOpt (Binary Minus (exprOpt exp1) (exprOpt exp2))

exprOpt (Binary Mult (NatLit 0) ex) = NatLit 0
exprOpt (Binary Mult ex (NatLit 0)) = NatLit 0
exprOpt (Binary Mult (NatLit 1) ex) = exprOpt ex
exprOpt (Binary Mult ex (NatLit 1)) = exprOpt ex
exprOpt (Binary Mult (NatLit n1) (NatLit n2)) = NatLit (n1 * n2)
exprOpt (Binary Mult exp1 exp2) = exprOpt (Binary Mult (exprOpt exp1) (exprOpt exp2))

exprOpt (Binary Div exp (NatLit 1)) = exprOpt exp
exprOpt (Binary Div (NatLit n1) (NatLit n2)) = NatLit (div n1 n2)
exprOpt (Binary Div exp1 exp2) = exprOpt (Binary Div (exprOpt exp1) (exprOpt exp2))

exprOpt (Binary Mod (NatLit n1) (NatLit n2)) = NatLit (mod n1 n2)
exprOpt (Binary Mod exp1 exp2) = exprOpt (Binary Mod (exprOpt exp1) (exprOpt exp2))

exprOpt (Unary Neg (NatLit n)) = NatLit (-n)
exprOpt (Unary Neg exp) = exprOpt (Unary Neg (exprOpt exp))


--OTRAS

exprOpt (Assign n exp) = Assign n (exprOpt exp)

