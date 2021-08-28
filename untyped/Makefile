APP			= main
OCAMLC		= ocamlc
OCAMLDEP	= ocamldep

INCLUDES 		= 
OCAMLFLAGS 		= $(INCLUDES)
OCAMLOPTFLAGS 	= $(INCLUDES)

# OBJS の順番を変えるとコンパイルできなくなることに注意（面倒くさい）
OBJS 			= support.cmo syntax.cmo core.cmo parser.cmo lexer.cmo main.cmo
LEXEROBJS = support.cmo support.cmi
DEPEND 		= lexer.ml parser.ml

all: .depend $(APP)

include .depend

$(APP) : $(OBJS)
	$(OCAMLC) -o $@ $(OCAMLFLAGS) $(OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

parser.ml: parser.mly
	ocamlyacc -v $<

lexer.ml: lexer.mll 
	ocamllex $<

# Clean up
.PHONY: clean
clean:
	rm -f main
	rm -f *.cm[iox]
	rm -f lexer.ml
	rm -f parser.ml
	rm -f parser.mli
	rm parser.output

# Dependencies
.depend: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend