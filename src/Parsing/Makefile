## File generated by the BNF Converter (bnfc 2.9.4).

# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : TestMacchiato

# Rules for building the parser.

AbsMacchiato.hs LexMacchiato.x ParMacchiato.y PrintMacchiato.hs TestMacchiato.hs : macchiato.cf
	bnfc --haskell --functor macchiato.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

TestMacchiato : AbsMacchiato.hs LexMacchiato.hs ParMacchiato.hs PrintMacchiato.hs TestMacchiato.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

distclean : clean
	-rm -f AbsMacchiato.hs AbsMacchiato.hs.bak ComposOp.hs ComposOp.hs.bak DocMacchiato.txt DocMacchiato.txt.bak ErrM.hs ErrM.hs.bak LayoutMacchiato.hs LayoutMacchiato.hs.bak LexMacchiato.x LexMacchiato.x.bak ParMacchiato.y ParMacchiato.y.bak PrintMacchiato.hs PrintMacchiato.hs.bak SkelMacchiato.hs SkelMacchiato.hs.bak TestMacchiato.hs TestMacchiato.hs.bak XMLMacchiato.hs XMLMacchiato.hs.bak ASTMacchiato.agda ASTMacchiato.agda.bak ParserMacchiato.agda ParserMacchiato.agda.bak IOLib.agda IOLib.agda.bak Main.agda Main.agda.bak macchiato.dtd macchiato.dtd.bak TestMacchiato LexMacchiato.hs ParMacchiato.hs ParMacchiato.info ParDataMacchiato.hs Makefile


# EOF
