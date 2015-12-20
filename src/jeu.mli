type etat = EnCours | Joueur1Gagnant | Joueur2Gagnant | Egalite

type pioche = Dictionnaire.lettre list
		     
type joueur = { nom : string ;
		mutable mots : Dictionnaire.mot list;
		mutable score : int ;
		mutable main : Dictionnaire.lettre list
	      }

type typejoueur = Joueur1 | Joueur2
			      
type partie = { mutable joueur1 : joueur ;
		mutable joueur2 : joueur;
		mutable dico : Dictionnaire.dico;
		mutable pioche : pioche;
		mutable etat : etat;
		mutable joueurCourant : typejoueur
	      }



exception Partie_termine
	    
val creer_partie : string -> string -> string -> partie

val jouerTour : partie -> partie

val put_partie : partie -> unit 
