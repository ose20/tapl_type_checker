APP			= main
OCAMLC		= ocamlc
OCAMLDEP	= ocamldep

INCLUDES 		= 
OCAMLFLAGS 		= $(INCLUDES)
OCAMLOPTFLAGS 	= $(INCLUDES)

OBJS 	= lexer.cmo parser.cmo core.cmo support.cmo syntax.cmo main.cmo
DEPEND 	= lexer.ml parser.ml

all: .depend $(APP)

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

# Dependencies
.depend: $(DEPEND)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend