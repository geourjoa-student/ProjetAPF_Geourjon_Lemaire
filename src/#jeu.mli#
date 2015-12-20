type etat = EnCours | Joueur1Gagnant | Joueur2Perdant

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
		mutable etat : etat
	      }


	    
val creer_partie : string -> string -> string -> partie

val jouerTourJ1 : partie -> partie

val jouerTourJ2 : partie -> partie
