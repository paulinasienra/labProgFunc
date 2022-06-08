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
checkProgram (Program mb) = checkNombres mb []

checkNombres :: MainBody -> [Name] -> [Error]
checkNombres [] acc = []
checkNombres (Decl vd:xs) acc = if checkVarDef vd acc then Duplicated m:checkNombres xs acc 
                                 else checkNombres xs (m:acc)
                                 where m = obtenerVar vd
checkNombres (Com (If ex b1 b2):ms) acc = checkExprNombres ex acc ++ checkBody b1 acc ++ checkBody b2 acc ++ checkNombres ms acc
checkNombres (Com (While ex b1):ms) acc = checkExprNombres ex acc ++ checkBody b1 acc ++ checkNombres ms acc
checkNombres (Com (PutChar ex):ms) acc = checkExprNombres ex acc ++ checkNombres ms acc
checkNombres (Com (StmtExpr ex):ms) acc = checkExprNombres ex acc ++ checkNombres ms acc

obtenerVar :: VarDef -> Name
obtenerVar (VarDef _ m) = m

memberName :: Name -> [Name] -> Bool
memberName m acc = (/=[])$filter(==m) acc

checkExprNombres :: Expr -> [Name] -> [Error]
checkExprNombres (Var c) acc | not (memberName c acc) = [Undefined c]
                             | otherwise = []
checkExprNombres (Unary _ c) acc = checkExprNombres c acc
checkExprNombres (Binary _ c e) acc = checkExprNombres c acc ++ checkExprNombres e acc
checkExprNombres (Assign n e) acc | memberName n acc = checkExprNombres e acc
                                  | otherwise = Undefined n : checkExprNombres e acc
checkExprNombres _ acc = []

checkVarDef :: VarDef -> [Name] -> Bool
checkVarDef (VarDef _ m) = memberName m 

checkBody :: Body -> [Name] -> [Error]
checkBody [] acc =  []
checkBody (If e b1 b2:xs) acc = checkExprNombres e acc ++ checkBody b1 acc ++ checkBody b2 acc ++ checkBody xs acc
checkBody (While e b:xs) acc = checkExprNombres e acc ++ checkBody b acc ++ checkBody xs acc
checkBody (StmtExpr e:xs) acc = checkExprNombres e acc ++ checkBody xs acc
checkBody (PutChar e:xs) acc = checkExprNombres e acc ++ checkBody xs acc

desparseo :: Either String Program -> [Error]
desparseo (Left _) = []
desparseo (Right p) = checkProgram p

parseoCheck :: String -> [Error]
parseoCheck s = desparseo $ parser s