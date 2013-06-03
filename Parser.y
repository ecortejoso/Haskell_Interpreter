{
module Parser where

import Lexer
import Abstract

}

%name parser
%error { syntaxError }
%tokentype { Token }
%token
  verdad                          	{ (TkTrue $$)       }  
  falso                             { (TkFalse $$)      }
  listvac                           { (TkListvac $$)    }
  concat                            { (TkConcat $$)     }
  menos                             { (TkMenos $$)      }
--  negativo                          { (TkNegativo $$)   }
  mas                               { (TkMas $$)        }
  mult                              { (TkMult $$)       }
  div                               { (TkDiv $$)        }
  mayor                             { (TkMayor $$)      }
  mayorig                           { (TkMayorIg $$)    }
  menor                             { (TkMenor $$)      }
  menorig                           { (TkMenorIg $$)    }
  igual                             { (TkIgual $$)      }
  noig                              { (TkNoIg $$)       }
  neg                               { (TkNeg $$)        }
  or                                { (TkOr $$)         }
  and                               { (TkAnd $$)        }
  fun                               { (TkFun $$)        }
  flecha                            { (TkFlecha $$)     }
  barra                             { (TkBarra $$)      }
  nuf                               { (TkNuf $$)        }
  if                                { (TkIf $$)         }
  then                              { (TkThen $$)       }
  else                              { (TkElse $$)       }
  fi                                { (TkFi $$)         }
  let                               { (TkLet $$)        }
  in                                { (TkIn $$)         }
  tel                               { (TkTel $$)        }
  pareni                            { (TkParentIzq $$)  }
  parend                            { (TkParentDer $$)  }
  num                               { (TkNum _ $$)     }
  id                                { (TkId _ $$)       }


%left or
%left and
%right neg
%nonassoc noig
%nonassoc menor menorig mayor mayorig igual
%left menos mas
%left mult div
%left NEGATIVO
%right concat
%left APLI
%nonassoc verdad falso
%left num id pareni parend listvac


%%

S: E                           { $1 }

E: A                           { $1 }
 | E concat E                  { Lista $1 $3 }
 | menos E %prec NEGATIVO        { Negativo $2 }
 | E mult E                    { Mult $1 $3 }
 | E div E                     { Div $1 $3 } 
 | E mas E                     { Mas $1 $3 }
 | E menos E                   { Menos $1 $3 }
 | E menor E                   { Menor $1 $3 }
 | E menorig E                 { Menorig $1 $3 }
 | E mayor E                   { Mayor $1 $3 }
 | E mayorig E                 { Mayorig $1 $3 }
 | E igual E                   { Igual $1 $3 }
 | E noig E                    { Noig $1 $3 }
 | neg E                       { Neg $2 }
 | E and E                     { And $1 $3 }
 | E or E                      { Or $1 $3 }
 | fun FA nuf                  { Fun $2 }
 | let P igual E in E tel      { Let $2 $4 $6 }
 | if E then E else E fi       { If $2 $4 $6 }


A: A A         %prec APLI      { Aplica $1 $2 }          
 | C                           { Termino $1 }
 | V                           { Termino $1 }
 | pareni E parend             { Expr $2 }
 
V: id                          { Var $1 }

C: num                         { Num $1 }
 | verdad                      { Booleano True }
 | falso                       { Booleano False }
 | listvac                     { Listavac }

FA: PS flecha E                { ListPatExp $1 $3 }
  | FA barra PS flecha E       { ListaPatternExp $1 $3 $5}
 
PS: P                          { ListPat $1 }
  | PS P                       { ListaPattern $1  $2 }

P: C                           { Pattern $1 }
 | V                           { Pattern $1 }
 | P concat P                  { LPat $1 $3 }
 | pareni P parend             { Patr $2 }



{

syntaxError :: [Token] -> a
syntaxError (t:ts) = error $ 
                      "Error de sintaxis en el Token " ++ (show t) ++ "\n" ++
                      "Seguido de: " ++ (unlines $ map show $ take 3 ts)


} 
