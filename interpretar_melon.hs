{-
   /Programa Principal/

  Traductores e Interpretadores CI3725
  
  Integrantes:

  04-36838: Maria Gabriela Chacon
  04-36867: Eduardo Cortejoso

 -}
module Main (main) where

import System
import System.IO
import System.Environment (getArgs)
import Lexer
import Parser
import Interpreter
{-
   @main@

   Función principal.
 -}

main =
	do
		args <- getArgs
		fileName <- getFilename args
		contents <- readFile fileName
		print $ interpreter $ parser $ lexer contents

getFilename args =	if null(args) then
											do 
												hSetBuffering stdout NoBuffering
												putStr "Archivo a Interpretar: "
												fileName <- getLine
												return fileName
										else
											do
												return (head args)
