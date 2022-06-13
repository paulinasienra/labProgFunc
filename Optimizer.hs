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
-- Logré que funcionara para If y While, hay que adaptarlo al resto

-- Neg, Equ, Less creo que estan mal, Equ, Less creo que no se tendrían que optimizar, porque no tienen ni nulo ni neutro
-- en Neg creo que esta mal tener NatLit (-n)
-- Por esto último hay que controlar los resultados negativos

--Se supone que primero se optimiza la subexpresión izquierda
--luego la derecha, y después la raíz
exprOpt :: Expr -> Expr
exprOpt (NatLit i) = NatLit i
exprOpt (CharLit c)  = CharLit c
exprOpt GetChar = GetChar 
exprOpt (Var v) = Var v

--OPERADORES LÓGICOS
exprOpt (Binary Or (NatLit n) ex) | n /= 0 = NatLit n
                                  | otherwise = exprOpt ex
exprOpt (Binary Or ex (NatLit n)) | n /= 0 = NatLit n
                                  | otherwise = exprOpt ex
exprOpt (Binary Or exp1 exp2) = Binary Or (exprOpt exp1) (exprOpt exp2)

exprOpt (Binary And (NatLit 0) ex) = NatLit 0
exprOpt (Binary And ex (NatLit 0)) = NatLit 0
exprOpt (Binary And (NatLit n) ex) = exprOpt ex
exprOpt (Binary And ex (NatLit n)) = exprOpt ex
exprOpt (Binary And exp1 exp2) = Binary And (exprOpt exp1) (exprOpt exp2)

exprOpt (Binary Equ (NatLit n1) (NatLit n2)) | n1 == n2 = NatLit 1
                                             | otherwise  = NatLit 0
exprOpt (Binary Equ exp1 exp2) = Binary Equ (exprOpt exp1) (exprOpt exp2)

exprOpt (Binary Less (NatLit n1) (NatLit n2)) | n1 < n2 = NatLit 1
                                              | otherwise = NatLit 0
exprOpt (Binary Less exp1 exp2) = Binary Less (exprOpt exp1) (exprOpt exp2)

exprOpt (Unary Not (NatLit x)) | x /= 0      = NatLit 0
                               | otherwise  = NatLit 1
exprOpt (Unary Not expr) = Unary Not (exprOpt expr)

--OPERADORES ARITMÉTICOS
exprOpt (Binary Plus (NatLit 0) ex) = exprOpt ex
exprOpt (Binary Plus ex (NatLit 0)) = exprOpt ex
exprOpt (Binary Plus (NatLit n1) (NatLit n2)) = NatLit (n1 + n2)
exprOpt (Binary Plus exp1 exp2) = Binary Plus (exprOpt exp1) (exprOpt exp2)

exprOpt (Binary Minus exp (NatLit 0)) = exprOpt exp
exprOpt (Binary Minus (NatLit n1) (NatLit n2)) = NatLit (n1 - n2) -- Controlar negativo
exprOpt (Binary Minus exp1 exp2) = Binary Minus (exprOpt exp1) (exprOpt exp2)

exprOpt (Binary Mult (NatLit 0) ex) = NatLit 0
exprOpt (Binary Mult ex (NatLit 0)) = NatLit 0
exprOpt (Binary Mult (NatLit 1) ex) = exprOpt ex
exprOpt (Binary Mult ex (NatLit 1)) = exprOpt ex
exprOpt (Binary Mult (NatLit n1) (NatLit n2)) = NatLit (n1 * n2) -- Controlar negativo
exprOpt (Binary Mult exp1 exp2) = Binary Mult (exprOpt exp1) (exprOpt exp2)

exprOpt (Binary Div exp (NatLit 1)) = exprOpt exp
exprOpt (Binary Div (NatLit n1) (NatLit n2)) = NatLit (div n1 n2) -- Controlar negativo
exprOpt (Binary Div exp1 exp2) = Binary Div (exprOpt exp1) (exprOpt exp2)

exprOpt (Binary Mod (NatLit n1) (NatLit n2)) = NatLit (mod n1 n2)
exprOpt (Binary Mod exp1 exp2) = Binary Mod (exprOpt exp1) (exprOpt exp2)

exprOpt (Unary Neg (NatLit n)) = NatLit (-n)
exprOpt (Unary Neg exp) = Unary Neg (exprOpt exp)


--OTRAS

exprOpt (Assign n exp) = Assign n (exprOpt exp)

esNatLit :: Expr -> Bool
esNatLit (NatLit _) = True
esNatLit _ = False 

stmtOpt :: Stmt -> [Stmt]
stmtOpt (If (NatLit n) r e) | n == 0 = e
                            | n /= 0 = r
                            | otherwise  = [If (NatLit n) r e]
stmtOpt (If exp b1 b2) | esNatLit (exprOpt exp) = stmtOpt (If (exprOpt exp) b1 b2)
                       | otherwise = [If (exprOpt exp) b1 b2]

stmtOpt (While (NatLit n) r ) | n == 0 = []
                              | n /= 0 = r
                              | otherwise  = [While (NatLit n) r]
stmtOpt (While exp b1) | esNatLit (exprOpt exp) = stmtOpt (While (exprOpt exp) b1)
                       | otherwise = [While (exprOpt exp) b1]

stmtOpt (StmtExpr exp) = [StmtExpr (exprOpt exp)]

stmtOpt (PutChar exp) = [PutChar (exprOpt exp)]