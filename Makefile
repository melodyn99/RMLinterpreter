MODULES=dwt eval io lexer parser pretty serialize types
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
REPL=rml_repl.byte
RUN=rml_interpreter.byte rml_server.byte
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build

build:
	$(OCAMLBUILD) $(OBJECTS)

all: build

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST)
	./test.byte

check:
	bash checkenv.sh
	bash checktypes.sh

shell:
	-@$(OCAMLBUILD) $(RUN) $(REPL)
	-@pkill -9 -f rml_server.byte || true
	-@./rml_repl.byte ./rml_server.byte ./rml_interpreter.byte
	-@pkill -9 -f rml_server.byte || true

repl:
	-@$(OCAMLBUILD) $(RUN) $(REPL)
	-@pkill -9 -f rml_server.byte || true
	-@./rml_repl.byte ./rml_server.byte ./rml_interpreter.byte
	-@pkill -9 -f rml_server.byte || true

run:
	$(OCAMLBUILD) $(RUN)
	@read -p "Which file would you like to read? " FILENAME; \
	./rml_server.byte "./rml_interpreter.byte" $$FILENAME

clean:
	ocamlbuild -clean

zip:
	mkdir ./interp
	mkdir ./interp/rml
	cp -a rml/list rml/async ./interp/rml
	cp eval.ml test.ml ./interp
	zip -r interp.zip ./interp
	rm -r ./interp
