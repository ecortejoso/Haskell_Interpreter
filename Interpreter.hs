module Interpreter (interpreter) where

import Lexer
import Parser
import Abstract

interpreter :: Exp -> Exp
interpreter x = eval [] x

type Tabla = [(Patron, Exp)]


eval :: Tabla -> Exp -> Exp
eval env (Termino x) = evalVarTerm env x
eval env (Mas x y) = (Termino (evalCalc 1 env x y)) 
eval env (Menos x y) = (Termino (evalCalc 2 env x y)) 
eval env (Mult x y) = (Termino (evalCalc 3 env x y)) 
eval env (Div x y) = (Termino (evalCalc 4 env x y)) 
eval env (And x y) = (Termino (evalCalc 5 env x y)) 
eval env (Or x y) = (Termino (evalCalc 6 env x y)) 
eval env (Igual x y) = (Termino (evalCalc 7 env x y)) 
eval env (Noig x y) = (Termino (evalCalc 8 env x y))
eval env (Menor x y) = (Termino (evalCalc 9 env x y)) 
eval env (Menorig x y) = (Termino (evalCalc 10 env x y)) 
eval env (Mayor x y) = (Termino (evalCalc 11 env x y))
eval env (Mayorig x y) = (Termino (evalCalc 12 env x y))
eval env (Expr x) = eval env x
eval env (If x y z) = evalIf env x y z
eval env (Neg x) = evalNot env x
eval env (Lista x y) = (Lista (eval env x) (eval env y))

eval env (Let p e1 e2 ) = evalLet env p e1 e2 (Num 1) 

evalLet :: Tabla -> Patron -> Exp -> Exp -> Term -> Exp
evalLet env p e1 e2 t1 = let env1 = extend env p (Termino t1) in
                                     let v1 = eval env1 e1 in
                                        eval (replace env1 p v1) e2

{-                                        
eval env (fun p1->e1|...|pn->en) = (CLS env (p1, e1):: ... ::(pn,en)::[])
eval env e1 e2 = apply (eval env e1) (eval env e2)
-}

evalVarTerm :: Tabla -> Term -> Exp
evalVarTerm env (Var x) = lookupi x env
evalVarTerm env x = (Termino (evalTerm env x))

evalTerm :: Tabla -> Term -> Term
evalTerm _ Listavac = Listavac
evalTerm _ (Booleano True) = Booleano True
evalTerm _ (Booleano False) = Booleano False
evalTerm _ (Num a) = Num a

evalCalc :: Int -> Tabla -> Exp -> Exp -> Term
evalCalc 1 env (Termino x) (Termino y) = evalSuma env x y
evalCalc 2 env (Termino x) (Termino y) = evalMenos env x y
evalCalc 3 env (Termino x) (Termino y) = evalMult env x y
evalCalc 4 env (Termino x) (Termino y) = evalDiv env x y
evalCalc 5 env (Termino x) (Termino y) = evalAnd env x y
evalCalc 6 env (Termino x) (Termino y) = evalOr env x y
evalCalc 7 env (Termino x) (Termino y) = evalIgual env x y
evalCalc 8 env (Termino x) (Termino y) = evalNoIgual env x y
evalCalc 9 env (Termino x) (Termino y) = evalMenor env x y
evalCalc 10 env (Termino x) (Termino y) = evalMenorIgual env x y
evalCalc 11 env (Termino x) (Termino y) = evalMayor env x y
evalCalc 12 env (Termino x) (Termino y) = evalMayorIgual env x y
evalCalc z env x y = evalCalc z env (eval env x) (eval env y)

evalSuma :: Tabla -> Term -> Term -> Term
evalSuma _ (Num a) (Num b) = Num (a + b) 
evalSuma env (Var a) (Num b) = evalSuma env (esTerm (lookupi a env)) (Num b) 
evalSuma env (Num a) (Var b) = evalSuma env (Num a) (esTerm (lookupi b env))
evalSuma env (Var a) (Var b) = evalSuma env (esTerm (lookupi a env)) (esTerm (lookupi b env)) 
evalSuma _ x y = error ("Error de tipos en: Suma " ++ show(x) ++"+"++ show(y))

evalMenos :: Tabla -> Term -> Term -> Term
evalMenos _ (Num a) (Num b) = Num (a - b) 
evalMenos env (Var a) (Num b) = evalMenos env (esTerm (lookupi a env)) (Num b) 
evalMenos env (Num a) (Var b) = evalMenos env (Num a) (esTerm (lookupi b env))
evalMenos env (Var a) (Var b) = evalMenos env (esTerm (lookupi a env)) (esTerm (lookupi b env)) 
evalMenos _ x y = error ("Error de tipos en: Resta " ++ show(x) ++"-"++ show(y))

evalMult :: Tabla -> Term -> Term -> Term
evalMult _ (Num a) (Num b) = Num (a * b) 
evalMult env (Var a) (Num b) = evalMult env (esTerm (lookupi a env)) (Num b) 
evalMult env (Num a) (Var b) = evalMult env (Num a) (esTerm (lookupi b env))
evalMult env (Var a) (Var b) = evalMult env (esTerm (lookupi a env)) (esTerm (lookupi b env)) 
evalMult _ x y = error ("Error de tipos en: Multiplicacion " ++ show(x) ++"*"++ show(y))

esTerm :: Exp -> Term
esTerm (Termino a) = a

evalDiv :: Tabla -> Term -> Term -> Term
evalDiv _ (Num a) (Num b) = Num (div a b) 
evalDiv env (Var a) (Num b) = evalDiv env (esTerm (lookupi a env)) (Num b) 
evalDiv env (Num a) (Var b) = evalDiv env (Num a) (esTerm (lookupi b env))
evalDiv env (Var a) (Var b) = evalDiv env (esTerm (lookupi a env)) (esTerm (lookupi b env)) 
evalDiv _ x y = error ("Error de tipos en: Division " ++ show(x) ++"/"++ show(y))

evalAnd :: Tabla -> Term -> Term -> Term
evalAnd _ (Booleano a) (Booleano b) = Booleano (a && b)
evalAnd env (Var a) (Booleano b) = evalAnd env (esTerm (lookupi a env)) (Booleano b) 
evalAnd env (Booleano a) (Var b) = evalAnd env (Booleano a) (esTerm (lookupi b env))
evalAnd env (Var a) (Var b) = evalAnd env (esTerm (lookupi a env)) (esTerm (lookupi b env))  
evalAnd _ x y = error ("Error de tipos en: " ++ show(x) ++" AND "++ show(y))

evalOr :: Tabla -> Term -> Term -> Term
evalOr _ (Booleano a) (Booleano b) = Booleano (a || b)
evalOr env (Var a) (Booleano b) = evalOr env (esTerm (lookupi a env)) (Booleano b) 
evalOr env (Booleano a) (Var b) = evalOr env (Booleano a) (esTerm (lookupi b env))
evalOr env (Var a) (Var b) = evalOr env (esTerm (lookupi a env)) (esTerm (lookupi b env))   
evalOr _ x y = error ("Error de tipos en: " ++ show(x) ++" OR "++ show(y))

evalIgual :: Tabla -> Term -> Term -> Term
evalIgual _ (Booleano a) (Booleano b) = Booleano (a == b) 
evalIgual _ (Num a) (Num b) = Booleano (a == b) 
evalIgual env (Var a) (Booleano b) = evalIgual env (esTerm (lookupi a env)) (Booleano b) 
evalIgual env (Booleano a) (Var b) = evalIgual env (Booleano a) (esTerm (lookupi b env))
evalIgual env (Var a) (Num b) = evalIgual env (esTerm (lookupi a env)) (Num b) 
evalIgual env (Num a) (Var b) = evalIgual env (Num a) (esTerm (lookupi b env))
evalIgual env (Var a) (Var b) = evalIgual env (esTerm (lookupi a env)) (esTerm (lookupi b env))  
evalIgual _ x y = error ("Error de tipos en: IGUAL " ++ show(x) ++" = "++ show(y))

evalNoIgual :: Tabla -> Term -> Term -> Term
evalNoIgual _ (Booleano a) (Booleano b) = Booleano (a /= b) 
evalNoIgual _ (Num a) (Num b) = Booleano (a /= b) 
evalNoIgual env (Var a) (Booleano b) = evalNoIgual env (esTerm (lookupi a env)) (Booleano b) 
evalNoIgual env (Booleano a) (Var b) = evalNoIgual env (Booleano a) (esTerm (lookupi b env))
evalNoIgual env (Var a) (Num b) = evalNoIgual env (esTerm (lookupi a env)) (Num b) 
evalNoIgual env (Num a) (Var b) = evalNoIgual env (Num a) (esTerm (lookupi b env))
evalNoIgual env (Var a) (Var b) = evalNoIgual env (esTerm (lookupi a env)) (esTerm (lookupi b env))  
evalNoIgual _ x y = error ("Error de tipos en: NO IGUAL " ++ show(x) ++" <> "++ show(y))

evalMenor :: Tabla -> Term -> Term -> Term
evalMenor _ (Num a) (Num b) = Booleano (a < b) 
evalMenor env (Var a) (Num b) = evalMenor env (esTerm (lookupi a env)) (Num b) 
evalMenor env (Num a) (Var b) = evalMenor env (Num a) (esTerm (lookupi b env))
evalMenor env (Var a) (Var b) = evalMenor env (esTerm (lookupi a env)) (esTerm (lookupi b env)) 
evalMenor _ x y = error ("Error de tipos en: MENOR " ++ show(x) ++" < "++ show(y))

evalMenorIgual :: Tabla -> Term -> Term -> Term
evalMenorIgual _ (Num a) (Num b) = Booleano (a <= b) 
evalMenorIgual env (Var a) (Num b) = evalMenorIgual env (esTerm (lookupi a env)) (Num b) 
evalMenorIgual env (Num a) (Var b) = evalMenorIgual env (Num a) (esTerm (lookupi b env))
evalMenorIgual env (Var a) (Var b) = evalMenorIgual env (esTerm (lookupi a env)) (esTerm (lookupi b env)) 
evalMenorIgual _ x y = error ("Error de tipos en: MENOR O IGUAL " ++ show(x) ++" <= "++ show(y))

evalMayor :: Tabla -> Term -> Term -> Term
evalMayor _ (Num a) (Num b) = Booleano (a > b)
evalMayor env (Var a) (Num b) = evalMayor env (esTerm (lookupi a env)) (Num b) 
evalMayor env (Num a) (Var b) = evalMayor env (Num a) (esTerm (lookupi b env))
evalMayor env (Var a) (Var b) = evalMayor env (esTerm (lookupi a env)) (esTerm (lookupi b env))  
evalMayor _ x y = error ("Error de tipos en: MAYOR " ++ show(x) ++" > "++ show(y))

evalMayorIgual :: Tabla -> Term -> Term -> Term
evalMayorIgual _ (Num a) (Num b) = Booleano (a <= b)
evalMayorIgual env (Var a) (Num b) = evalMayorIgual env (esTerm (lookupi a env)) (Num b) 
evalMayorIgual env (Num a) (Var b) = evalMayorIgual env (Num a) (esTerm (lookupi b env))
evalMayorIgual env (Var a) (Var b) = evalMayorIgual env (esTerm (lookupi a env)) (esTerm (lookupi b env))  
evalMayorIgual _ x y = error ("Error de tipos en: MAYOR O IGUAL " ++ show(x) ++" >= "++ show(y))

evalIf :: Tabla -> Exp -> Exp -> Exp -> Exp
evalIf env x y z = if matchIf env x then eval env y else eval env z

matchIf :: Tabla -> Exp -> Bool
matchIf env (Termino x) = matchIfaux x 
matchIf env x = matchIf env (eval env x) 

matchIfaux :: Term -> Bool
matchIfaux (Booleano True) = True
matchIfaux (Booleano False) = False
matchIfaux x = error ("Error de tipos en: CONDICION " ++ show(x))

evalNot :: Tabla -> Exp -> Exp
evalNot env (Termino x) = Termino (evalNotAux env x)
evalNot env x = evalNot env (eval env x)

evalNotAux :: Tabla -> Term -> Term
evalNotAux _ (Booleano True) = Booleano False
evalNotAux _ (Booleano False) = Booleano True
evalNotAux env (Var a) = evalNotAux env (esTerm (lookupi a env))
evalNotAux _ x = error ("Error de tipos en: NOT " ++ show(x))

evalNegativo :: Tabla -> Exp -> Exp
evalNegativo env (Termino x) = Termino (evalNegativoAux env x)
evalNegativo env x = evalNegativo env (eval env x)

evalNegativoAux :: Tabla -> Term -> Term
evalNegativoAux _ (Num a) = Num (-a)
evalNegativoAux env (Var a) = evalNegativoAux env (esTerm (lookupi a env))
evalNegativoAux _ x = error ("Error de tipos en: NEGATIVO " ++ show(x))

evalLista :: Tabla -> Exp -> Exp -> Exp
evalLista env x y = (Lista (eval env x) (eval env y))


{-
apply (CLS env ((p,e):cases) ) v = if (match v p) then
                                       eval (extend env p v) e
                                  else
                                       apply (CLS env cases) v
-}
match :: Exp -> Exp -> Bool
match (Termino x) (Termino y) = matchTerm x y
--match (v1:v2) (p1:p2) = (match v1 p1) && (match v2 p2)
match _ _ = False    

matchTerm :: Term -> Term -> Bool
matchTerm (Booleano True) (Booleano True) = True
matchTerm (Booleano False) (Booleano False) = True
matchTerm (Listavac) (Listavac) = True
matchTerm _ (Var x) = True



lookupi :: String -> Tabla -> Exp
--lookupi (Var x) ((x,v):env) = v
lookupi x ((y,v):env) = if x == (getStr y) then v else lookupi x env
--lookupi Var x (p1::p2, v1::v2)::env = lookupi x (p1, v1) ? lookupi x (p2, v2) ? lookupi x env
lookupi x [] =  error ("Error de lookup")

getStr :: Patron -> String
getStr (Pattern a) = getStrAux a
getStr _ = error ("Error de lookup") 

getStrAux :: Term -> String
getStrAux (Var a) = a
getStrAux _ = error ("Error de lookup") 

--convertirFun (Funcion (UnParametro p e))


extend :: Tabla -> Patron -> Exp -> Tabla
extend [] p1 v1 = [(p1, v1)]
extend xs p1 v1 = (p1, v1):xs

replace :: Tabla -> Patron -> Exp -> Tabla
replace [] _ _ = []
replace xs p s = replaceaux xs [] p s

replaceaux :: Tabla -> Tabla -> Patron -> Exp -> Tabla
replaceaux [] ys _ _ = ys
-- replaceaux ((p, b):xs) ys p s = replaceaux xs (p,s):ys p s
replaceaux ((a, b):xs) ys p s = if a == p then replaceaux xs ((p,s):ys) p s else replaceaux xs ((a,b):ys) p s 

