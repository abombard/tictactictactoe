NAME = game
SOURCES = $(INTERFACES:.mli=.ml) Game.ml
INTERFACES =

CAMLC = ocamlc.opt
CAMLOPT = ocamlopt.opt
CAMLDEP = ocamldep

LIBS =
WITHGRAPHICS = graphics.cma -cclib -lGraphics

all: depend $(NAME)

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

IOBJS = $(INTERFACES:.mli=.cmi)
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(IOBJS) $(OBJS)
	$(CAMLC) -o $@ $(LIBS) $(INTERFACES) $(OBJS)

$(NAME).opt: $(IOBJS) $(OPTOBJS)
	$(CAMLOPT) -o $@ $(LIBS:.cma=.cmxa) $(INTERFACES) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

clean:
	rm -f *.cm[iox]
	rm -f $(SOURCES:.ml=.o)
	rm -f $(NAME).o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

.depend:
	$(CAMLDEP) $(SOURCES) > .depend

depend: .depend

re: fclean all

include .depend
