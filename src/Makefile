EXEC=jarnac
CAMLCFLAGS=-c 
LIBS=
OBJS= type.cmo dictionnaire.cmo main.cmo

all: $(EXEC) clean

#$(EXEC): $(OBJS)
#	ocamlc -o $(EXEC) $(LIBS) $(OBJS)

#%.cmo: %.ml %.cmi
#	ocamlc -c $<

jarnac.cmo : jarnac.ml jeu.cmo affichage.cmo
	ocamlc -c jarnac.ml jeu.cmo

jarnac : jeu.cmo jarnac.cmo
	ocamlc -o jarnac jeu.cmo jarnac.cmo

jeu.cmo : jeu.ml jeu.cmi dictionnaire.cmo
	ocamlc -c jeu.ml 

jeu.cmi : jeu.mli dictionnaire.cmi
	ocamlc -c jeu.mli 

dictionnaire.cmo : dictionnaire.ml dictionnaire.cmi 
	ocamlc -c dictionnaire.ml 

dictionnaire.cmi : dictionnaire.mli
	ocamlc -c dictionnaire.mli

affichage.cmo : affichage.ml affichage.cmi dictionnaire.cmo
	ocamlc -c affichage.ml

affichage.cmi : affichage.mli
	ocamlc -c affichage.mli

clean:
	rm -rf *.cmi *.cmo *~ 
