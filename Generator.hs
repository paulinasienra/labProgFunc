-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE GENERACIÓN DE CÓDIGO DE MÁQUINA

module Generator where

import Syntax
import MachineLang

-- Implementar
generate :: Program -> Code
generate (Program mb) = genCodMaq mb

genCodMaq :: MainBody -> Code
genCodMaq [] = [SKIP]
genCodMaq (Decl _:xs) = genCodMaq xs
genCodMaq (Com stmt:xs) = fst (stmtCodMaq stmt) ++ genCodMaq xs

stmtCodMaq :: Stmt -> (Code, Integer)
stmtCodMaq (StmtExpr (Assign n e)) = (fst (exprCodMaq e) ++ [STORE n], snd (exprCodMaq e) + 1)
stmtCodMaq (StmtExpr e) = exprCodMaq e
stmtCodMaq (PutChar e) = (fst (exprCodMaq e) ++ [WRITE], snd (exprCodMaq e) + 1) -- 
stmtCodMaq (If e b1 b2) = (fst (exprCodMaq e) ++ JMPZ (fromIntegral(snd(bodyCodMaq b1))+ 1): fst(bodyCodMaq b1) ++ JUMP (fromIntegral(snd(bodyCodMaq b2))) : fst(bodyCodMaq b2), snd(bodyCodMaq b1) + snd(bodyCodMaq b2) + 2) -- evaluar expr
stmtCodMaq (While e b) = (fst (exprCodMaq e) ++ JMPZ (fromIntegral(snd(bodyCodMaq b))+ 1) :fst(bodyCodMaq b) ++ [JUMP (-(fromIntegral(snd(exprCodMaq e)) + fromIntegral(snd(bodyCodMaq b))))], snd(bodyCodMaq b) + snd(exprCodMaq e) + 2) -- lo mismo

bodyCodMaq :: Body -> (Code, Integer)
bodyCodMaq [] = ([],0)
bodyCodMaq (st:xs)  =  (fst (stmtCodMaq st) ++ fst (bodyCodMaq xs), snd (stmtCodMaq st) + snd (bodyCodMaq xs))


exprCodMaq :: Expr -> (Code, Integer)
exprCodMaq (Var n) = ([LOAD n],1)
exprCodMaq (CharLit c) = ([PUSH (toInteger$fromEnum c)],1) -- ASCII de c, ponele
exprCodMaq (NatLit n) = ([PUSH n],1)
exprCodMaq GetChar = ([READ],1)
exprCodMaq (Assign n e) = (fst (exprCodMaq e) ++ [STORE n]++[LOAD n], snd (exprCodMaq e) + 2)


-- Aritmetica
exprCodMaq (Binary Plus e1 e2) = (fst(exprCodMaq e2) ++ fst(exprCodMaq e1) ++ [ADD], snd (exprCodMaq e1) + snd (exprCodMaq e2) + 1)
exprCodMaq (Binary Minus e1 e2) = (fst(exprCodMaq e2) ++ fst(exprCodMaq e1) ++ [SUB], snd (exprCodMaq e1) + snd (exprCodMaq e2) + 1)
exprCodMaq (Binary Mult e1 e2) = (fst(exprCodMaq e2) ++ fst(exprCodMaq e1) ++ [MUL], snd (exprCodMaq e1) + snd (exprCodMaq e2) + 1)
exprCodMaq (Binary Div e1 e2) = (fst(exprCodMaq e2) ++ fst(exprCodMaq e1) ++ [DIV], snd (exprCodMaq e1) + snd (exprCodMaq e2) + 1)
exprCodMaq (Binary Mod e1 e2) = (fst(exprCodMaq e2) ++ fst(exprCodMaq e1) ++ [MOD], snd (exprCodMaq e1) + snd (exprCodMaq e2) + 1)
exprCodMaq (Unary Neg e) = (fst(exprCodMaq e) ++ [NEG], snd (exprCodMaq e) + 1)

-- Logicos
exprCodMaq (Binary Less e1 e2) = ( fst (exprCodMaq e2) ++ fst(exprCodMaq e1) ++ CMP: PUSH 1: ADD: JMPZ 2: PUSH 0: JUMP 1: [PUSH 1],snd (exprCodMaq e1) + snd (exprCodMaq e2) + 7)
exprCodMaq (Binary Equ e1 e2) = ( fst (exprCodMaq e2) ++ fst(exprCodMaq e1) ++ CMP: JMPZ 2: PUSH 0: JUMP 1: [PUSH 1],snd (exprCodMaq e1) + snd (exprCodMaq e2) + 5)
exprCodMaq (Unary Not e) = (fst (exprCodMaq e) ++ JMPZ 2: PUSH 0: JUMP 1: [PUSH 1], snd (exprCodMaq e) + 4)
exprCodMaq (Binary And e1 e2) = ( fst (exprCodMaq e2) ++ fst(exprCodMaq e1) ++ ADD: PUSH 2: CMP:JMPZ 2:PUSH 0:JUMP 1:[PUSH 1] ,snd (exprCodMaq e1) + snd (exprCodMaq e2) + 7)
exprCodMaq (Binary Or e1 e2) = ( fst (exprCodMaq e2) ++ fst(exprCodMaq e1) ++ ADD: JMPZ 2: PUSH 1: JUMP 1: [PUSH 0],snd (exprCodMaq e1) + snd (exprCodMaq e2) + 5)


desparseoGen :: Either String Program -> Code
desparseoGen (Left _) = []
desparseoGen (Right p) = generate p

parseoGenerador :: String -> Code
parseoGenerador s = desparseoGen $ parser s

prueba = "int x; int y; x = 1; x = 1; x = x || 0;"