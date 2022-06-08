-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS

module TypeChecker where

import Syntax


data Error = Duplicated      Name
           | Undefined       Name
           | Expected        Type Type

instance Show Error where
   show (Duplicated      n)  = "Duplicated definition: " ++ n
   show (Undefined       n)  = "Undefined: " ++ n
   show (Expected    ty ty') = "Expected: " ++ show ty
                             ++ " Actual: " ++ show ty'

type Env = [(Name, Type)]

-- Implementar
checkProgram :: Program -> [Error]
checkProgram (Program MB) = case checkNombres MB [] of
               [] -> checkTipos t []

checkNombres :: MainBody -> [Name] -> [Error]
checkNombres [] acc = []
checkNombres (Decl VarDef _ m:ms) acc = if (member m acc) then Duplicated m:checkNombres ms acc
                           else checkNombres ms (m:acc)
checkNombres (Com If ex b1 b2:ms) acc = checkExprNombres ex acc ++ checkBody b1 acc ++ checkBody b2 acc ++ checkNombres ms acc
checkNombres (Com While ex b1:ms) acc = checkExprNombres ex acc ++ checkBody b1 acc ++ checkNombres ms acc
checkNombres (Com PutChar ex:ms) acc = checkExprNombres ex acc ++ checkNombres ms acc

member :: Name -> [Name] -> Boolean
member m acc = (/=[]).filter(==m) acc

checkExprNombres :: Expr -> [Name] -> [Error]
checkExprNombres = Undefined

checkBody :: Body -> [Name] -> [Error]
checkBody = Undefined

