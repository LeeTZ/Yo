OBJS = ast.cmo parser.cmo scanner.cmo interpret.cmo bytecode.cmo compile.cmo execute.cmo yo.cmo

TESTS = \
arith1 \
arith2 \
fib \
for1 \
func1 \
func2 \
func3 \
gcd \
gcd2 \
global1 \
hello \
if1 \
if2 \
if3 \
if4 \
ops1 \
var1 \
while1

# Choose one
YACC = ocamlyacc
# YACC = menhir --explain

TARFILES = Makefile testall.sh scanner.mll parser.mly \
	ast.ml bytecode.ml interpret.ml compile.ml execute.ml yo.ml \
	$(TESTS:%=tests/test-%.mc) \
	$(TESTS:%=tests/test-%.out)

yo : $(OBJS)
	ocamlc -o yo $(OBJS)

.PHONY : test
test : yo testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

yo.tar.gz : $(TARFILES)
	cd .. && tar czf yo/yo.tar.gz $(TARFILES:%=yo/%)

.PHONY : clean
clean :
	rm -f yo parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff

# Generated by ocamldep *.ml *.mli
ast.cmo: 
ast.cmx: 
bytecode.cmo: ast.cmo 
bytecode.cmx: ast.cmx 
compile.cmo: bytecode.cmo ast.cmo 
compile.cmx: bytecode.cmx ast.cmx 
execute.cmo: bytecode.cmo ast.cmo 
execute.cmx: bytecode.cmx ast.cmx 
interpret.cmo: ast.cmo 
interpret.cmx: ast.cmx 
yo.cmo: scanner.cmo parser.cmi interpret.cmo execute.cmo compile.cmo \
    bytecode.cmo ast.cmo 
yo.cmx: scanner.cmx parser.cmx interpret.cmx execute.cmx compile.cmx \
    bytecode.cmx ast.cmx 
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 
