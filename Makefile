DOCDIR   = doc
BINARIES = interpretar_melon
OBJECTS  = *.o *.hi
SOURCES  = interpretar_melon.hs Abstract.hs Interpreter.hs
MODULES  = Lexer.x Parser.y

all:	$(BINARIES)
  
interpretar_melon: $(SOURCES) $(MODULES)
	hmake interpretar_melon -package containers
	
clean:
	rm -rf Lex*.hs Parser.hs $(OBJECTS) $(BINARIES)
