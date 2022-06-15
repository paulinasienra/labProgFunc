-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE OPTIMIZACIÓN
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Optimizer where

import Syntax


-- Implementar
optimize :: Program -> Program
optimize (Program mb) = Program (optProg mb)

optProg :: MainBody -> MainBody
optProg [] = []
optProg (Decl vd:xs) = Decl vd : optProg xs
optProg (Com stm:xs) | not$null (stmtOpt stm) = Com (head(stmtOpt stm)) : optProg xs
                     | otherwise = optProg xs

optParser :: Either String Program -> Program
optParser (Left _) = Program []
optParser (Right p) = optimize p

parseoCheck :: String -> Program  
parseoCheck s = optParser $ parser s


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
exprOpt (Binary Or (NatLit 0) ex) = exprOpt ex
exprOpt (Binary Or ex (NatLit 0)) = exprOpt ex
exprOpt (Binary Or (NatLit n) ex) = NatLit n
exprOpt (Binary Or exp1 exp2) | snd (esNatLit e1) && fst (esNatLit e1) /= 0 = NatLit $fst (esNatLit e1)
                              | snd (esNatLit e1) && fst (esNatLit e1) == 0 = e2
                              | snd (esNatLit e2) && fst (esNatLit e2) /= 0 = NatLit $fst (esNatLit e2)
                              | snd (esNatLit e2) && fst (esNatLit e2) == 0 = e1
                              | otherwise = Binary Or e1 e2
                              where
                                e1 = exprOpt exp1
                                e2 = exprOpt exp2


exprOpt (Binary And (NatLit 0) ex) = NatLit 0
exprOpt (Binary And ex (NatLit 0)) = NatLit 0
exprOpt (Binary And (NatLit n) ex) = exprOpt ex
exprOpt (Binary And exp1 exp2) | snd (esNatLit e1) && fst (esNatLit e1) /= 0 = e2
                               | snd (esNatLit e1) && fst (esNatLit e1) == 0 = NatLit 0
                               | snd (esNatLit e2) && fst (esNatLit e2) /= 0 = e1
                               | snd (esNatLit e2) && fst (esNatLit e2) == 0 = NatLit 0
                               | otherwise = Binary And e1 e2
                               where
                                 e1 = exprOpt exp1
                                 e2 = exprOpt exp2


exprOpt (Binary Equ exp1 exp2) = Binary Equ (exprOpt exp1) (exprOpt exp2)

exprOpt (Binary Less exp1 exp2) = Binary Less (exprOpt exp1) (exprOpt exp2)

exprOpt (Unary Not expr) = Unary Not (exprOpt expr)

--OPERADORES ARITMÉTICOS
exprOpt (Binary Plus (NatLit 0) ex) = exprOpt ex
exprOpt (Binary Plus ex (NatLit 0)) = exprOpt ex
exprOpt (Binary Plus (Unary Neg exp1) (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Unary Neg (NatLit (fst (esNatLit e1) + fst (esNatLit e2))))-- (-2) + (-4) = -2 -4 = -6
                                                        | otherwise = Binary Plus (Unary Neg e1) (Unary Neg e2)
                                                       where
                                                         e1 = exprOpt exp1
                                                         e2 = exprOpt exp2
exprOpt (Binary Plus (Unary Neg exp1) exp2) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Binary Minus (NatLit (fst (esNatLit e2))) (NatLit (fst (esNatLit e1))))-- (-2) + 3 = 1
                                            | otherwise = Binary Plus (Unary Neg e1) e2
                                           where
                                             e1 = exprOpt exp1
                                             e2 = exprOpt exp2
exprOpt (Binary Plus exp1 (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Binary Minus (NatLit (fst (esNatLit e1))) (NatLit (fst (esNatLit e2))))-- 2 + (-5) = -3
                                            | otherwise = Binary Plus e1 (Unary Neg exp2)
                                           where
                                             e1 = exprOpt exp1
                                             e2 = exprOpt exp2
exprOpt (Binary Plus exp1 exp2) | snd (esNatLit e1) && snd (esNatLit e2) = NatLit $ fst (esNatLit e1) + fst (esNatLit e2)
                                | otherwise = Binary Plus e1 e2
                               where
                                 e1 = exprOpt exp1
                                 e2 = exprOpt exp2

exprOpt (Binary Minus ex (NatLit 0)) = exprOpt ex
exprOpt (Binary Minus (NatLit 0) ex) = Unary Neg (exprOpt ex)
exprOpt (Binary Minus (Unary Neg exp1) (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Binary Plus (Unary Neg exp1) (NatLit (fst (esNatLit e2))))-- (-x) - (-y) ==> (-x) + y
                                                         | otherwise = Binary Minus (Unary Neg e1) (Unary Neg e2)
                                                        where
                                                          e1 = exprOpt exp1
                                                          e2 = exprOpt exp2
exprOpt (Binary Minus (Unary Neg exp1) exp2) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Unary Neg (NatLit $abs(fst (esNatLit e2) + fst (esNatLit e1))))
                                             | otherwise = Binary Minus (Unary Neg e1) e2
                                            where
                                              e1 = exprOpt exp1
                                              e2 = exprOpt exp2
exprOpt (Binary Minus exp1 (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt(Binary Plus (NatLit (fst (esNatLit e1))) (NatLit (fst (esNatLit e2))))
                                             | otherwise = Binary Minus e1 (Unary Neg exp2)
                                            where
                                              e1 = exprOpt exp1
                                              e2 = exprOpt exp2
exprOpt (Binary Minus exp1 exp2) | (snd (esNatLit e1) && snd (esNatLit e2)) && fst (esNatLit e1) < fst (esNatLit e2) = exprOpt (Unary Neg (NatLit $abs (fst (esNatLit e1) - fst (esNatLit e2))))
                                 | snd (esNatLit e1) && snd (esNatLit e2) = NatLit (fst (esNatLit e1) - fst (esNatLit e2))
                                 | otherwise = Binary Minus e1 e2
                                where
                                  e1 = exprOpt exp1
                                  e2 = exprOpt exp2

exprOpt (Binary Mult (NatLit 0) ex) = NatLit 0
exprOpt (Binary Mult ex (NatLit 0)) = NatLit 0
exprOpt (Binary Mult (NatLit 1) ex) = exprOpt ex
exprOpt (Binary Mult ex (NatLit 1)) = exprOpt ex -- Controlar negativo
exprOpt (Binary Mult (Unary Neg exp1) (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = NatLit (fst (esNatLit e1) * fst (esNatLit e2))
                                                        | otherwise = Binary Mult (Unary Neg e1) (Unary Neg e2)
                                                       where
                                                         e1 = exprOpt exp1
                                                         e2 = exprOpt exp2
exprOpt (Binary Mult (Unary Neg exp1) exp2) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Unary Neg (NatLit (fst (esNatLit e1) * fst (esNatLit e2))))
                                            | otherwise = Binary Mult (Unary Neg e1) e2
                                           where
                                             e1 = exprOpt exp1
                                             e2 = exprOpt exp2
exprOpt (Binary Mult exp1 (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Unary Neg (NatLit (fst (esNatLit e1) * fst (esNatLit e2))))
                                            | otherwise = Binary Mult e1 (Unary Neg exp2)
                                           where
                                             e1 = exprOpt exp1
                                             e2 = exprOpt exp2
exprOpt (Binary Mult exp1 exp2) | snd (esNatLit e1) && snd (esNatLit e2) = NatLit (fst (esNatLit e1) * fst (esNatLit e2))
                                | otherwise = Binary Mult e1 e2
                               where
                                 e1 = exprOpt exp1
                                 e2 = exprOpt exp2

exprOpt (Binary Div exp (NatLit 1)) = exprOpt exp
exprOpt (Binary Div (NatLit n1) (NatLit n2)) = NatLit (div n1 n2) -- Controlar negativo igual que Mult, por los signos
exprOpt (Binary Div (Unary Neg exp1) (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = NatLit (div (fst (esNatLit e1)) (fst (esNatLit e2)))
                                                       | otherwise = Binary Div (Unary Neg e1) (Unary Neg e2)
                                                     where
                                                       e1 = exprOpt exp1
                                                       e2 = exprOpt exp2
exprOpt (Binary Div (Unary Neg exp1) exp2) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt(Unary Neg (NatLit (div (fst (esNatLit e1))  (fst (esNatLit e2)))))
                                           | otherwise = Binary Div (Unary Neg e1) e2
                                          where
                                           e1 = exprOpt exp1
                                           e2 = exprOpt exp2
exprOpt (Binary Div exp1 (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Unary Neg (NatLit (div (fst (esNatLit e1))  (fst (esNatLit e2)))))
                                            | otherwise = Binary Div e1 (Unary Neg exp2)
                                           where
                                             e1 = exprOpt exp1
                                             e2 = exprOpt exp2
exprOpt (Binary Div exp1 exp2) | snd (esNatLit e1) && snd (esNatLit e2)  = NatLit (div (fst (esNatLit e1)) (fst (esNatLit e2)))
                               | otherwise = Binary Div e1 e2
                              where
                                e1 = exprOpt exp1
                                e2 = exprOpt exp2

exprOpt (Binary Mod (NatLit n1) (NatLit n2)) = NatLit (mod n1 n2)
exprOpt (Binary Mod (Unary Neg exp1) (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Unary Neg(NatLit (mod (fst (esNatLit e1)) (fst (esNatLit e2)))))-- por si da 0 lo optimizo al Neg
                                                       | otherwise = Binary Mod (Unary Neg e1) (Unary Neg e2)
                                                      where
                                                       e1 = exprOpt exp1
                                                       e2 = exprOpt exp2
exprOpt (Binary Mod (Unary Neg exp1) exp2) | snd (esNatLit e1) && snd (esNatLit e2) = NatLit (mod (fst (esNatLit e1)) (fst (esNatLit e2)))
                                           | otherwise = Binary Mod (Unary Neg e1) e2
                                          where
                                            e1 = exprOpt exp1
                                            e2 = exprOpt exp2
exprOpt (Binary Mod exp1 (Unary Neg exp2)) | snd (esNatLit e1) && snd (esNatLit e2) = exprOpt (Unary Neg (NatLit (mod (fst (esNatLit e1)) (fst (esNatLit e2)))))
                                            | otherwise = Binary Mod e1 (Unary Neg exp2)
                                           where
                                             e1 = exprOpt exp1
                                             e2 = exprOpt exp2
exprOpt (Binary Mod exp1 exp2) | snd (esNatLit e1) && snd (esNatLit e2) = NatLit (mod (fst (esNatLit e1)) (fst (esNatLit e2)))
                               | otherwise = Binary Mod e1 e2
                              where
                                e1 = exprOpt exp1
                                e2 = exprOpt exp2

exprOpt (Unary Neg (Unary Neg exp)) = exp
exprOpt (Unary Neg exp) | snd (esNatLit e1) && fst (esNatLit e1) == 0 = NatLit 0
                        | otherwise = Unary Neg e1
                        where
                         e1 = exprOpt exp


--OTRAS

exprOpt (Assign n exp) = Assign n (exprOpt exp)

esNatLit :: Expr -> (Integer,Bool)
esNatLit (NatLit n) = (n,True)
esNatLit _ = (0,False)


stmtOpt :: Stmt -> [Stmt]
stmtOpt (If (NatLit n) r e) | n == 0 = e
                            | n /= 0 = r
                            | otherwise  = [If (NatLit n) r e]
stmtOpt (If exp b1 b2) | snd (esNatLit (exprOpt exp)) = stmtOpt (If (exprOpt exp) (bodyOpt b1) (bodyOpt b2))
                       | otherwise = [If (exprOpt exp) (bodyOpt b1) (bodyOpt b2)]

stmtOpt (While (NatLit n) r ) | n == 0 = []
                              | n /= 0 = r
                              | otherwise  = [While (NatLit n) r]
stmtOpt (While exp b1) | snd (esNatLit (exprOpt exp)) = stmtOpt (While (exprOpt exp) (bodyOpt b1))
                       | otherwise = [While (exprOpt exp) (bodyOpt b1)]

stmtOpt (StmtExpr exp) = [StmtExpr (exprOpt exp)]

stmtOpt (PutChar exp) = [PutChar (exprOpt exp)]

bodyOpt :: Body -> Body
bodyOpt = concatMap stmtOpt