SOURCES = snappy.idl compress.ml
RESULT = compress

PACKS = batteries

CLIBS = /usr/lib/libsnappy.a
LDFLAGS = -lstdc++

all: native-code native-code-library

-include OCamlMakefile
