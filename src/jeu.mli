type pioche = Dictionnaire.lettre list
		     
type joueur = { nom : string ;
		mutable mots : Dictionnaire.mot list;
		mutable score : int ;
		mutable main : Dictionnaire.lettre list
	      }
type partie = { mutable joueur1 : joueur ;
		mutable joueur2 : joueur;
		mutable dico : Dictionnaire.dico;
		mutable pioche : pioche;
		mutable termine : int (* 0 : en cours, 1 : joueur1 gagnant, 2 :joueur 2 gagnant *)
	      }

val creer_partie : string -> string -> string -> partie
