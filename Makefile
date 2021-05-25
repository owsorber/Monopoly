MODULES=authors game player board cards stockmarket input main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
GUI=gui.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind \
-plugin-tag 'package(bisect_ppx-ocamlbuild)'

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

bisect: clean bisect-test
	bisect-ppx-report html

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) \
		&& ./$(TEST)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

zip:
	zip monopoly.zip *.ml* *.json *.sh *.txt _tags .merlin .ocamlformat .ocamlinit Makefile

docs: docs-public docs-private open-docs
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,graphics \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,graphics \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

open-docs: ./open-docs.sh

clean:
	ocamlbuild -clean _doc.public _doc.private _coverage bisect*.coverage