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



let lettresDisponibles = ['A';'A';'A';'A';'A';'A';'A';'A';'A';'A';'A';'A';'A';'A';
			  'B';'B';'B';'B';
			  'C';'C';'C';'C';'C';'C';'C';
			  'D';'D';'D';'D';'D';
			  'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';'E';
			  'F';'F';
			  'G';'G';'G';'G';
			  'H';'H';
			  'I';'I';'I';'I';'I';'I';'I';'I';'I';'I';'I';
			  'J';
			  'K';
			  'L';'L';'L';'L';'L';'L';
			  'M';'M';'M';'M';'M';
			  'N';'N';'N';'N';'N';'N';'N';'N';'N';
			  'O';'O';'O';'O';'O';'O';'O';'O';
			  'P';'P';'P';'P';
			  'Q';
			  'R';'R';'R';'R';'R';'R';'R';'R';'R';'R';
			  'S';'S';'S';'S';'S';'S';'S';
			  'T';'T';'T';'T';'T';'T';'T';'T';'T';
			  'U';'U';'U';'U';'U';'U';'U';'U';
			  'V';'V';
			  'W';
			  'X';
			  'Y';
			  'Z';'Z']
					
					    
			  
		

let rec longueur ( l : 'a list) : int =
  match l with
  | [] -> 0
  | t::reste -> 1 + (longueur reste)

		

let rec ieme_element ( l : 'a list ) ( n : int ) : 'a list * 'a =
  match l with
  | [] -> failwith "Element non présent"
  | t::reste -> if n=0 then
		  (reste,t)
		else
		  let (lr,x) = (ieme_element reste (n-1)) in
		  ((t::lr),x)

let _ = Random.self_init ()
		
let creer_pioche : pioche =
  let rec sub_creer_pioche ( l : Dictionnaire.lettre list ) ( n : int ) : pioche =
    match n with
    |0->l
    |n->let (lr,x) = (ieme_element l (Random.int (n))) in
	x::(sub_creer_pioche lr (n-1))
  in
  sub_creer_pioche lettresDisponibles (longueur lettresDisponibles)
  
		
let creer_dico ( n : string ) : Dictionnaire.dico =
  if n="vide" then
    Dictionnaire.dico_vide ()
  else
    if n="tout" then
      Dictionnaire.dico_plein ()
    else Dictionnaire.of_file n

		
let rec piocher (p:pioche)(i:int):Dictionnaire.lettre list=
  match p,i with
  |[],i->failwith "plus de lettres"
  |l::fin,0->l::fin
  |l::fin,j->l::(piocher fin (j-1))

let creer_joueur ( nj : string ) : joueur =
  { nom = nj;
    mots = [];
    score = 0;
    main = [];
  }

	    
let creer_partie ( nj1 : string ) ( nj2 : string ) ( nDico : string ) : partie =
  { joueur1 = creer_joueur nj1 ;
    joueur2 = creer_joueur nj2 ;
    dico = creer_dico nDico ;
    pioche = creer_pioche;
    etat = EnCours ;
  }


let jouerTourJ1 ( p : partie ) : partie =
  p

let jouerTourJ2 ( p : partie ) : partie =
  p    
    



 
