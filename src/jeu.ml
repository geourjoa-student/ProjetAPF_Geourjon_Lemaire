type etat = EnCours | Joueur1Gagnant | Joueur2Perdant
		  
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

exception Fin_de_tour

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

(* Fonctions d'affichage *)
			   
let rec put_mot ( m : Dictionnaire.mot) : unit =
  match m with
  | [] -> print_string "\n"
  | t::reste ->
     begin
       print_string "| ";
       print_char t;
       print_string " ";
       put_mot reste
     end

let put_main ( m : Dictionnaire.mot ) : unit =
  print_string "| main : ";
  put_mot m
	  


let rec put_mots ( l : Dictionnaire.mot list ) : unit =
  match l with
  | [] -> ()
  | t::reste ->
     begin
       put_mot t ;
       put_mots reste
     end


let put_joueur ( j : joueur ) : unit =
  begin
    print_string ("Joueur : " ^ j.nom ^ " Score : ") ; print_int j.score ; print_string "\n" ;
    print_string "-------------------------------------\n";
    print_string "|   |   | 9 | 16| 25| 36| 49| 64| 81|\n";
    print_string "-------------------------------------\n";
    print_string "| Mots :-\n";
    print_string "-------------------------------------\n";
    put_mots j.mots;
    print_string "-------------------------------------\n";
    put_main j.main ;
    print_string "-------------------------------------\n";
  end


let put_partie ( p : partie ) : unit =
  begin
    print_string " \n \n*** Jarnac *** \n\n" ;
    if p.joueurCourant = Joueur1 then
      print_string "Joueur 1 joue : \n\n"
    else
      print_string "Joueur 2 joue : \n\n" ;
    put_joueur p.joueur1 ;
    print_string "\n" ;
    put_joueur p.joueur2
  end					    
			  
(* Fonction liée à la gestion de la pioche *)		

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

let rec piocher (m:Dictionnaire.lettre list)(p:pioche)(i:int):Dictionnaire.lettre list*pioche=
  match i with
  |0->(m,p)
  |j-> match m,p with
    |m,[]->failwith"pioche vide" (* pas de règles dans le jeu prévue, pas supposé se produire *)
    |m,p1::pfin-> piocher (p1::m) pfin (i-1)
	

(* Fonction d'initialisation de partie *)
			 
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
      

let creer_joueur ( nj : string ) ( m : Dictionnaire.lettre list ): joueur =
  { nom = nj;
    mots = [];
    score = 0;
    main = m;
  }

	    
let creer_partie ( nj1 : string ) ( nj2 : string ) ( nDico : string ) : partie =
  let pioche = (creer_pioche) in
  let (nmain1, npioche1) = (piocher [] pioche 6) in
  let (nmain2, npioche2) = (piocher [] npioche1 6) in
  { pioche = npioche2;   
    joueur1 = creer_joueur nj1 nmain1 ;
    joueur2 = creer_joueur nj2 nmain2 ;
    dico = creer_dico nDico ; 
    etat = EnCours ;
    joueurCourant = Joueur1
  }

(* Moteur de jeu *)


let jouermot (mot:Dictionnaire.lettre list)(tab:Dictionnaire.mot list):Dictionnaire.mot list = tab@[mot]

let rec echangermot (oldm:Dictionnaire.lettre list)(newm:Dictionnaire.lettre list)(tab:Dictionnaire.mot list):Dictionnaire.mot list=
  match tab with
  |[]->[]
  |mot::fin->if mot=oldm then newm::fin else mot::(echangermot oldm newm fin)

(*Joueur volé, joueur voleur*)						    
let rec echangerjarnac (oldm:Dictionnaire.lettre list)(newm:Dictionnaire.lettre list)(tabOrigine:Dictionnaire.mot list)(tabDest:Dictionnaire.mot list):Dictionnaire.mot list*Dictionnaire.mot list=
  match tabOrigine with
  |[]->[],[]
  |mot::fin->if mot=oldm
    then fin,tabDest@[newm]
    else let (tab1,tab2)=echangerjarnac oldm newm fin tabDest in
	 mot::tab1,tab2

(*TODO*)
let jarnaquer ( p : partie) ( j : typejoueur) : partie =
  print_string "Le coup de jarnac n'est pas encore implémenté \n" ;
  raise Fin_de_tour		     

let faire_un_jarnac ( p : partie ) ( j : typejoueur ): partie =
  print_string "Voulez vous tenter un jarnac ? oui (o) ou non (n).\n(adversaire) " ;
  match read_line () with
  | y when y="y" -> jarnaquer p j
  | n when n="n" -> print_string "La main passe !\n" ;
		    raise Fin_de_tour
  | _ -> print_string "Commande incorrecte, la main passe !" ;
	 raise Fin_de_tour

(*TODO*)
let ajouter_un_mot ( p : partie ) (j : typejoueur) : partie =
  print_string "L'ajout de mot n'est pas encore implémenté \n" ;
  raise Fin_de_tour

(*TODO*)
let modifier_un_mot ( p : partie ) ( j : typejoueur ) : partie =
  print_string "La modification de mot n'est pas encore implémenté \n" ;
  raise Fin_de_tour
    

(*TODO rajouter le test qui permet de vérifier que la partie est terminé et qui jette l'exception*)
let terminer_partie ( p : partie) : partie =
  p





		     

(* TODO erreur de compilation sur la premiere ligne du match*)		     
let jouerTour ( par : partie ) : partie =
 let partie = ref par in
  try
     
      while true do
	print_string "Voulez vous passer (p),ajouter un mot (a) ou modifier un mot déjà présent (m) ? \n(joueur) " ;
	match read_line () with
	| p when p="p" -> partie := faire_un_jarnac !partie !partie.joueurCourant
	| a when a="a" -> partie := ajouter_un_mot (!partie) !partie.joueurCourant
	| m when m="m" ->  partie := modifier_un_mot (!partie) !partie.joueurCourant
	| _ -> print_string "Commande incorrecte, la main passe !" ;
	       raise Fin_de_tour
      done ;
      !partie (* Inutile, uniquement pour la compilation car on n'executera jamais cette ligne *)
    with Fin_de_tour -> terminer_partie !partie




 
   
  


