SOURCES = scanner.mll \
          parser.mly \
          common.mli \ 
          sast.ml \
          ast.ml \
          semantic.ml \
          parser.ml \
          scanner.ml \
          parser_test.ml \
          output.ml \
          typeReader.ml

OCAMLBUILD = ocamlbuild

all: 
	$(OCAMLBUILD) parser_test.native

clean:
	$(OCAMLBUILD) -clean


