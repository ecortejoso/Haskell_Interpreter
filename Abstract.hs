
module Abstract (
	Term(..),
	Exp(..),
	ListaPatron(..),
	Patron(..),
	ListaPatronExp(..),

) where

import Lexer
import Data.Char
import Data.Maybe
import Data.Map as Mapa

type Tabla = [(Patron, Exp)]

data Term = Var String 
			| Booleano Bool
			| Num Int
			| Listavac
			deriving (Eq)

data Exp = Termino Term   
				| Lista Exp Exp
				| Aplica Exp Exp
                | Neg Exp
                | And Exp Exp
				| Or  Exp Exp
				| Menor Exp Exp
				| Menorig Exp Exp
				| Mayor Exp Exp
				| Mayorig Exp Exp
				| Igual Exp Exp
				| Noig Exp Exp
				| Negativo Exp
				| Mas Exp Exp
                | Menos Exp Exp
                | Mult Exp Exp
                | Div Exp Exp
				| If Exp Exp Exp
				| Fun ListaPatronExp 
				| Let Patron Exp Exp
				| Expr Exp
				| CLS Tabla Exp
                deriving (Eq)

data ListaPatronExp = ListaPatternExp ListaPatronExp ListaPatron Exp 
				 | ListPatExp ListaPatron Exp
				 deriving (Eq)
				
data ListaPatron = ListaPattern ListaPatron Patron 
				 | ListPat Patron
				 deriving (Eq)
					 
data Patron = Pattern Term
			| LPat Patron Patron
			| Patr Patron
			deriving (Eq)	

instance Show Term where
	show (a) = mostrarTerm a
	
mostrarTerm (Var a) = show(a)
mostrarTerm (Booleano True) = "true"
mostrarTerm (Booleano False) = "false"
mostrarTerm (Num a) = show(a)
mostrarTerm Listavac = "[]"

instance Show Exp where
	show (a) = mostrarExp a

mostrarExp (Termino a) = show(a)
mostrarExp (Lista a b) = show(a)++ "::"++show(b)
mostrarExp (Aplica a b) = "(APLICA "++show(a)++" "++show(b)++")"
mostrarExp (Neg a) = "(NO " ++show(a)++")"
mostrarExp (And a b) = "(AND "++show(a)++" "++show(b)++")"
mostrarExp (Or a b) = "(OR "++show(a)++" "++show(b)++")"
mostrarExp (Menor a b) = "(MENOR "++show(a)++" "++show(b)++")"
mostrarExp (Menorig a b) = "(MENOROIGUAL "++show(a)++" "++show(b)++")"
mostrarExp (Mayor a b) = "(MAYOR "++show(a)++" "++show(b)++")"
mostrarExp (Mayorig a b) = "(MAYOROIGUAL "++show(a)++" "++show(b)++")"
mostrarExp (Igual a b) = "(IGUAL "++show(a)++" "++show(b)++")"
mostrarExp (Noig a b) = "(DISTINTO "++show(a)++" "++show(b)++")"
mostrarExp (Negativo a) = "(NEGATIVO "++show(a)++")"
mostrarExp (Mas a b) = "(MAS "++show(a)++" "++show(b)++")"
mostrarExp (Menos a b) = "(MENOS "++show(a)++" "++show(b)++")"
mostrarExp (Mult a b) = "(PRODUCTO "++show(a)++" "++show(b)++")"
mostrarExp (Div a b) = "(COCIENTE "++show(a)++" "++show(b)++")"
mostrarExp (If a b c) = "(IF "++show(a)++" "++show(b)++" "++show(c)++")"
mostrarExp (Fun a) = "(FUN "++show(a)++")"
mostrarExp (Let a b c) = "(LET "++show(a)++" "++show(b)++" "++show(c)++")"
mostrarExp (Expr a) = "("++show(a)++")"

instance Show ListaPatronExp where
	show (a) = mostrarListaPatronExp a
	
mostrarListaPatronExp (ListaPatternExp a b c) = "( "++show(a)++" (LISTAPATRON "++show(b)++") "++show(c)++")"
mostrarListaPatronExp (ListPatExp a b) = "(LISTAPATRON "++show(a)++") "++show(b)++")"

instance Show ListaPatron where
	show (a) = mostrarListaPatron a
	
mostrarListaPatron (ListaPattern a b) = " "++show(a)++" "++" "++show(b)++" "
mostrarListaPatron (ListPat a) = " "++show(a)++" "

instance Show Patron where
	show (a) = mostrarPatron a

mostrarPatron (Pattern a) = "(PATRON "++show(a)++")"
mostrarPatron (LPat a b) = "(LISTA "++show(a)++" "++show(b)++")"
mostrarPatron (Patr a) = "("++show(a)++")"




