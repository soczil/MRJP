JVM = InscJVM
LLVM = InscLLVM
JVM_PATH = src/${JVM}.hs
LLVM_PATH = src/${LLVM}.hs
INSTANT = Instant
INSTANT_PATH = src/${INSTANT}.cf
GHC = ghc
BNFC = bnfc # /home/students/inf/PUBLIC/MRJP/bin/bnfc
PARSER_BUILD = parser_build

.PHONY: all

all: clean parser jvm llvm

parser: ${INSTANT_PATH}
	mkdir ${PARSER_BUILD} && \
	cd ${PARSER_BUILD} && \
	${BNFC} --haskell -d -m ../${INSTANT_PATH} && \
	make && \
	mv ${INSTANT} ../ && \
	cd .. && \
	rm -r -f ${PARSER_BUILD}

jvm: ${JVM_PATH}
	${GHC} -main-is ${JVM} ${JVM_PATH} -o insc_jvm

llvm: ${LLVM_PATH}
	${GHC} -main-is ${LLVM} ${LLVM_PATH} -o insc_llvm

clean:
	rm -r -f ${INSTANT} src/*.o src/*.hi insc_jvm insc_llvm
