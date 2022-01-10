BNFC = /home/students/inf/PUBLIC/MRJP/bin/bnfc
LATTE = Latte
LATTE_PATH = src/${LATTE}.cf
MAIN = src/Main.hs
TYPECHECKER = src/Typechecker.hs
COMPILER = src/Compiler.hs
ERRORS = src/Errors.hs
RUNTIME_PATH = lib/runtime.c
GHC = ghc
PARSER_BUILD = parser_build
EXEC_FILE = latc_llvm

.PHONY: all

all: clean parser runtime main

parser: ${LATTE_PATH}
	mkdir ${PARSER_BUILD} && \
	cd ${PARSER_BUILD} && \
	${BNFC} --haskell -d -m --functor ../${LATTE_PATH} && \
	make && \
	mv ${LATTE} ../ && \
	cd .. && \
	rm -r -f ${PARSER_BUILD}

runtime: ${RUNTIME_PATH}
	clang -emit-llvm -c ${RUNTIME_PATH} -o lib/runtime.bc

main: ${MAIN_PATH} ${TYPECHECKER} ${COMPILER} ${ERRORS}
	${GHC} -isrc ${MAIN} -o ${EXEC_FILE}

clean:
	rm -r -f ${LATTE} src/*.o src/*.hi ${EXEC_FILE} lib/*.bc
