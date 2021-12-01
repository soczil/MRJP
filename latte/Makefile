BNFC = /home/students/inf/PUBLIC/MRJP/bin/bnfc
LATTE = Latte
LATTE_PATH = src/${LATTE}.cf
MAIN = src/Main.hs
TYPECHECKER = src/Typechecker.hs
ERRORS = src/Errors.hs
GHC = ghc
PARSER_BUILD = parser_build
EXEC_FILE = latc

.PHONY: all

all: clean parser main

parser: ${LATTE_PATH}
	mkdir ${PARSER_BUILD} && \
	cd ${PARSER_BUILD} && \
	${BNFC} --haskell -d -m --functor ../${LATTE_PATH} && \
	make && \
	mv ${LATTE} ../ && \
	cd .. && \
	rm -r -f ${PARSER_BUILD}

main: ${MAIN_PATH} ${TYPECHECKER} ${ERRORS}
	${GHC} -isrc ${MAIN} -o ${EXEC_FILE}

clean:
	rm -r -f ${LATTE} src/*.o src/*.hi ${EXEC_FILE}
