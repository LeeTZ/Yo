SOURCES = scanner.mll \
          parser.mly \
          sast.ml \
          ast.ml \
          parser.ml \
          scanner.ml \
          parser_test.ml \
          semantic.ml
     
OCAMLBUILD = ocamlbuild

all: 
	$(OCAMLBUILD) parser_test.native semantic.native

clean:
	$(OCAMLBUILD) -clean


