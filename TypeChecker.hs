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
                     [] -> reverse (checkTipos mb [])
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
checkTipos (Com stat:xs) acc = checkTipos xs acc ++ checkBodyTipos [stat] acc 
-- Body = [Stmt]
-- Env = [(Name,Type)]

obtenerVarTipo :: VarDef -> (Name,Type)
obtenerVarTipo (VarDef t n) = (n,t)


--checkStmt :: Stmt -> Env -> [Error]
--checkStmt st = checkBodyTipos [st]

checkBodyTipos :: Body -> Env -> [Error]
checkBodyTipos [] _ = []
checkBodyTipos (If e b1 b2:xs) acc | fst tip == TyChar = checkBodyTipos xs acc ++ checkBodyTipos b2 acc ++ 
                                        checkBodyTipos b1 acc ++ snd tip ++ [Expected TyInt TyChar]
                                   | otherwise = checkBodyTipos xs acc ++ checkBodyTipos b2 acc ++ checkBodyTipos b1 acc ++ snd tip  
                                 where
                                    tip = checkExprTipos e acc
--   snd (checkExprTipos e acc) ++ checkBodyTipos b1 acc ++ checkBodyTipos b2 acc ++ checkBodyTipos xs acc
checkBodyTipos (While e b:xs) acc | fst tip == TyChar = checkBodyTipos xs acc ++ checkBodyTipos b acc ++ snd tip ++ [Expected TyInt TyChar] 
                                   | otherwise = checkBodyTipos xs acc ++ checkBodyTipos b acc ++ snd tip
                                 where
                                    tip = checkExprTipos e acc
checkBodyTipos (StmtExpr e:xs) acc = checkBodyTipos xs acc ++ snd (checkExprTipos e acc) 
checkBodyTipos (PutChar e:xs) acc | fst tip == TyInt  = checkBodyTipos xs acc ++ reverse (Expected TyChar TyInt:snd tip)
                                  | otherwise =  checkBodyTipos xs acc ++ snd tip
                                  where 
                                     tip = checkExprTipos e acc
                                  -- Agregar snd a los checkExprTipos y ponerlo aca


checkExprTipos :: Expr -> Env -> (Type,[Error])
checkExprTipos (Var nom) acc = (buscarTipo nom acc,[])

checkExprTipos (NatLit _) acc = (TyInt,[])

checkExprTipos (Binary Equ e1 e2) acc | fst t1 == fst t2 = (TyInt, snd t1 ++ snd t2)
                                      | otherwise = (TyInt,Expected (fst t1) (fst t2):snd t1 ++ snd t2)
                                      where 
                                         t1 = checkExprTipos e1 acc
                                         t2 = checkExprTipos e2 acc

checkExprTipos (Binary Less e1 e2) acc | fst t1 == fst t2 = (fst t1,snd t1 ++ snd t2)
                                       | otherwise = (fst t1,Expected (fst t1) (fst t2):snd t1 ++ snd t2)
                                       where
                                          t1 = checkExprTipos e1 acc
                                          t2 = checkExprTipos e2 acc

checkExprTipos (Binary _ e1 e2) acc | (fst t1 == TyInt) && (fst t2 == TyInt) = (TyInt,snd t1 ++ snd t2)
                                    | (fst t1 == TyChar) && (fst t2 == TyChar) = (TyInt,Expected TyInt TyChar:Expected TyInt TyChar:snd t1 ++ snd t2)
                                    | otherwise = (TyInt,Expected TyInt TyChar:snd t1 ++ snd t2)
                                    where
                                       t1 = checkExprTipos e1 acc
                                       t2 = checkExprTipos e2 acc

checkExprTipos (Unary _ e) acc | fst tip == TyChar = (TyInt,Expected TyInt TyChar:snd tip)
                               | otherwise = (TyInt,snd tip)
                               where 
                                  tip = checkExprTipos e acc

checkExprTipos (Assign n e) acc | buscarTipo n acc == fst tip = (buscarTipo n acc, snd tip)
                                | otherwise                                      = (buscarTipo n acc,Expected (buscarTipo n acc) (fst tip):snd tip)
                                where
                                   tip = checkExprTipos e acc
checkExprTipos _ _ = (TyChar,[])


buscarTipo :: Name -> Env -> Type
buscarTipo nom (x:xs) | nom == fst x = snd x
                      | otherwise =  buscarTipo nom xs
buscarTipo _ [] = TyInt -- Esto es necesario?