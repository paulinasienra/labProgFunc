-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS

module TypeChecker where

import Syntax
import GHC.Generics (Generic1(to1))


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
checkProgram (Program mb) = case checkNombres mb [] of
                     [] -> checkTipos mb []
                     err -> err

checkNombres :: MainBody -> [Name] -> [Error]
checkNombres [] acc = []
checkNombres (Decl vd:xs) acc = if checkVarDef vd acc then Duplicated m:checkNombres xs acc 
                                 else checkNombres xs (m:acc)
                                 where m = obtenerVar vd
checkNombres (Com (If ex b1 b2):ms) acc = checkExprNombres ex acc ++ checkBody b1 acc ++ checkBody b2 acc ++ checkNombres ms acc
checkNombres (Com (While ex b1):ms) acc = checkExprNombres ex acc ++ checkBody b1 acc ++ checkNombres ms acc
checkNombres (Com (PutChar ex):ms) acc = checkExprNombres ex acc ++ checkNombres ms acc
checkNombres (Com (StmtExpr ex):ms) acc = checkExprNombres ex acc ++ checkNombres ms acc

--Se podria cambiar lo de arriba por esto?
--checkNombres (Com stat:ms) acc = checkBody stat acc ++ checkNombres ms acc
--checkStmt :: Stmt -> Env -> [Error]
--checkStmt st = checkBody [st]

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






--SEGUNDO CHECKEO
checkTipos :: MainBody -> Env -> [Error]
checkTipos [] acc = []
checkTipos (Decl vd:xs) acc = checkTipos xs (obtenerVarTipo vd:acc)
checkTipos (Com stat:xs) acc = checkBodyTipos [stat] acc ++ checkTipos xs acc
-- Body = [Stmt]
-- Env = [(Name,Type)]

obtenerVarTipo :: VarDef -> (Name,Type)
obtenerVarTipo (VarDef t n) = (n,t)


--checkStmt :: Stmt -> Env -> [Error]
--checkStmt st = checkBodyTipos [st]

checkBodyTipos :: Body -> Env -> [Error]
checkBodyTipos [] _ = []
checkBodyTipos (If e b1 b2:xs) acc = snd (checkExprTipos e acc) ++ checkBodyTipos b1 acc ++ checkBodyTipos b2 acc ++ checkBodyTipos xs acc
checkBodyTipos (While e b:xs) acc = snd (checkExprTipos e acc) ++ checkBodyTipos b acc ++ checkBodyTipos xs acc
checkBodyTipos (StmtExpr e:xs) acc = snd (checkExprTipos e acc) ++ checkBodyTipos xs acc
checkBodyTipos (PutChar e:xs) acc | tip == TyInt  = Expected TyChar TyInt:snd (checkExprTipos e acc) ++ checkBodyTipos xs acc
                                  | otherwise = snd (checkExprTipos e acc) ++ checkBodyTipos xs acc
                                  where tip = fst (checkExprTipos e acc)
                                  -- Agregar snd a los checkExprTipos y ponerlo aca


checkExprTipos :: Expr -> Env -> (Type,[Error])
checkExprTipos (Var nom) acc = (buscarTipo nom acc,[])

checkExprTipos (NatLit _) acc = (TyInt,[])

checkExprTipos (Binary Equ e1 e2) acc | fst (checkExprTipos e1 acc) == fst (checkExprTipos e2 acc) = (fst(checkExprTipos e1 acc), snd (checkExprTipos e1 acc) ++ snd (checkExprTipos e2 acc))
                                      | otherwise = (fst (checkExprTipos e1 acc),Expected (fst (checkExprTipos e2 acc)) (fst (checkExprTipos e2 acc)):snd (checkExprTipos e1 acc) ++ snd (checkExprTipos e2 acc))

checkExprTipos (Binary Less e1 e2) acc | fst (checkExprTipos e1 acc) == fst (checkExprTipos e2 acc) = (fst(checkExprTipos e1 acc),snd (checkExprTipos e1 acc) ++ snd (checkExprTipos e2 acc))
                                       | otherwise = (fst (checkExprTipos e1 acc),Expected (fst (checkExprTipos e1 acc)) (fst (checkExprTipos e2 acc)):snd (checkExprTipos e1 acc) ++ snd (checkExprTipos e2 acc))

checkExprTipos (Binary _ e1 e2) acc | (fst (checkExprTipos e1 acc) == TyInt) && (fst (checkExprTipos e2 acc) == TyInt) = (TyInt,snd (checkExprTipos e1 acc) ++ snd (checkExprTipos e2 acc))
                                    | (fst (checkExprTipos e1 acc) == TyChar) && (fst (checkExprTipos e2 acc) == TyChar) = (TyInt,Expected TyInt TyChar:Expected TyInt TyChar:snd (checkExprTipos e1 acc) ++ snd (checkExprTipos e2 acc))
                                    | otherwise = (TyInt,Expected TyInt TyChar:snd (checkExprTipos e1 acc) ++ snd (checkExprTipos e2 acc))

checkExprTipos (Unary _ e) acc | fst(checkExprTipos e acc) == TyChar = (TyInt,Expected TyInt TyChar:snd (checkExprTipos e acc))
                               | otherwise = (TyInt,snd (checkExprTipos e acc))

checkExprTipos (Assign n e) acc | buscarTipo n acc == fst (checkExprTipos e acc) = (buscarTipo n acc, snd (checkExprTipos e acc))
                                | otherwise                                        = (buscarTipo n acc,Expected (buscarTipo n acc) (fst (checkExprTipos e acc)):snd (checkExprTipos e acc))
checkExprTipos _ _ = (TyChar,[])


buscarTipo :: Name -> Env -> Type
buscarTipo nom (x:xs) | nom == fst x = snd x
                      | otherwise =  buscarTipo nom xs
buscarTipo _ [] = TyInt -- Esto es necesario?