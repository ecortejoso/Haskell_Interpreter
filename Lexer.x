{
{-|
  /Analizador Lexicográfico/

  Traductores e Interpretadores CI3725
  
  Integrantes:

  04-36838: Maria Gabriela Chacon
  04-36867: Eduardo Cortejoso

-}

module Lexer (
  -- * Tipos exportados.
  -- ** /Tokens/ producidos por el Analizador Lexicográfico.
  Token(..),
  -- * Funciones exportadas.
  -- ** Analizador Lexicográfico.
  lexer 
) where
}

%wrapper "posn"

$digito = 0-9           -- UN digito
$digitomcero = 0-9
$letra  = [a-zA-Z]      -- UNA letra
$signo = [\+\-]

tokens :-

  $white+                                 ;
  true                                    {   \p s -> TkTrue (posNpos p)            }
  false                                   {   \p s -> TkFalse (posNpos p)           }
  "[]"                                    {   \p s -> TkListvac (posNpos p)         }
  "::"                                    {   \p s -> TkConcat (posNpos p)          }
  "-"                                   {   \p s -> TkMenos (posNpos p)           }
 -- " - "                                     {   \p s -> TkNegativo (posNpos p)        }
  \+                                      {   \p s -> TkMas (posNpos p)             }
  \*                                      {   \p s -> TkMult (posNpos p)            }
  \/                                      {   \p s -> TkDiv (posNpos p)             }
  ">"                                     {   \p s -> TkMayor (posNpos p)           }
  ">="                                    {   \p s -> TkMayorIg (posNpos p)         }
  "<"                                     {   \p s -> TkMenor (posNpos p)           }
  "<="                                    {   \p s -> TkMenorIg (posNpos p)         }
  "="                                     {   \p s -> TkIgual (posNpos p)           }
  "<>"                                    {   \p s -> TkNoIg (posNpos p)            }
  "!"                                     {   \p s -> TkNeg (posNpos p)             }
  "\/"                                    {   \p s -> TkOr (posNpos p)              }
  "/\"                                    {   \p s -> TkAnd (posNpos p)             }
  fun                                     {   \p s -> TkFun (posNpos p)             }
  "->"                                    {   \p s -> TkFlecha (posNpos p)          }
  "|"                                     {   \p s -> TkBarra (posNpos p)           }
  nuf                                     {   \p s -> TkNuf (posNpos p)             }
  if                                      {   \p s -> TkIf (posNpos p)              }
  then                                    {   \p s -> TkThen (posNpos p)            }
  else                                    {   \p s -> TkElse (posNpos p)            }
  fi                                      {   \p s -> TkFi (posNpos p)              }
  let                                     {   \p s -> TkLet (posNpos p)             }
  in                                      {   \p s -> TkIn (posNpos p)              }
  tel                                     {   \p s -> TkTel (posNpos p)             }
  \(                                      {   \p s -> TkParentIzq (posNpos p)       }
  \)                                      {   \p s -> TkParentDer (posNpos p)       }
  $digitomcero $digito*           {   \p s -> TkNum (posNpos p) (read s)    }
  $letra [ $letra $digito _ ]*            {   \p s -> TkId (posNpos p) s            }

{

data Token =
      TkTrue (Int,Int)  
    | TkFalse (Int,Int)
    | TkListvac (Int,Int)
    | TkConcat (Int,Int)
    | TkMenos (Int,Int)
	| TkNegativo (Int,Int)
    | TkMas (Int,Int)
    | TkMult (Int,Int)
    | TkDiv (Int,Int)
    | TkMayor (Int,Int)
    | TkMayorIg (Int,Int)
    | TkMenor (Int,Int)
    | TkMenorIg (Int,Int)
    | TkIgual (Int,Int)
    | TkNoIg (Int,Int)
    | TkNeg (Int,Int)
    | TkOr (Int,Int)
    | TkAnd (Int,Int)
    | TkFun (Int,Int)
    | TkFlecha (Int,Int)
    | TkBarra (Int,Int)
    | TkNuf (Int,Int)
    | TkIf (Int,Int)
    | TkThen (Int,Int)
    | TkElse (Int,Int)
    | TkFi (Int,Int)
    | TkLet (Int,Int)
    | TkIn (Int,Int)
    | TkTel (Int,Int)
    | TkParentIzq (Int,Int)
    | TkParentDer (Int,Int)
    | TkNum (Int,Int) Int
    | TkId (Int,Int) String
        deriving (Eq, Show)  


lexer :: String      -- ^ Cadena de caracteres @S@ a "tokenizar"
         -> [Token]  -- ^ Lista resultante de /tokens/ del tipo @Token@.

lexer s = if null(errors)
			 then
				tokens
			 else
				error (concat errors)
			 where
				(tokens,errors) = scanTokens s

scanTokens :: String -> ([Token],[String])
scanTokens str = go (alexStartPos,'\n',str) ([],[])
	where go inp@(pos,_,str) (ts,es) =
		case alexScan inp 0 of
			AlexEOF                -> (ts,es)
			AlexSkip  inp' len     -> go inp' (ts,es)
			AlexToken inp' len act -> (tsf,esf)
			  where
				  (ts',esf) = go inp' (ts,es)
				  tsf       = act pos (take len str) : ts'
			AlexError inp'         -> (tsf,esf)
				where
					(tsf,es')           = go (skipChar inp') (ts,es)
					esf                 = addError inp' es'
					skipChar (p,c,s)    = (alexMove p (head s),c,(tail s))
					addError (p,_,s) es = m : es
						where
							(l,c) = posNpos p
							m     = "\nLexer Error: ( Caracter : " ++ 
											[ head s ] ++
											" , linea : " ++ (show l) ++
											" , columna : " ++ (show c) ++ " ).\n"

posNpos :: AlexPosn -> (Int,Int)
posNpos (AlexPn _ l c) = (l,c)

}
