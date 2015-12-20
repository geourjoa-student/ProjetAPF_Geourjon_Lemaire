type dico =  Noeud of dico array * bool | Feuille
type lettre = char
type mot = lettre list
(* Fonctions internes *)
					    
(* a=0 , b=25 *)
let numero_lettre ( c : lettre ) : int =
  (int_of_char (Char.uppercase c)) - 65 ;;

let lettre_numero ( i : int ) : lettre =
  char_of_int (i+65)
	      

let string_to_list str =
  let rec loop i limit =
    if i = limit then []
    else (String.get str i) :: (loop (i + 1) limit)
  in
  loop 0 (String.length str);;
  
(*D'après la grille de jeu on ne dépassera jamais de mot de plus de 9 lettres*)
let longueur_max_mot = 9

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
    | [] -> res
    | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* Fonctions de manipulations de dictionnaire *)
      

(* Initialiser un dictionnaire arborescent de mot qui ne reconnait aucun mot*)
let dico_vide (): dico =		    
  let rec sub_dico_vide ( i : int) : dico =
    match i with
    | 0 -> Feuille
    | x -> let n = Noeud ((Array.make 26 (sub_dico_vide (i-1))),false)
	   in n
  in
  sub_dico_vide 10

(*TODO*) 
(* Initialiser un dictionnaire arborescent de mot qui reconnait tout les mots*)
let dico_plein (): dico =		    
  let rec sub_dico_plein ( i : int) : dico =
    match i with
    | 0 -> Feuille
    | x -> let n = Noeud ((Array.make 26 (sub_dico_plein (i-1))),true)
	   in n
  in
  sub_dico_plein 10		
		
let rec member ( mot : mot ) ( d : dico ) : bool =
  match d with
  | Feuille -> false
  | Noeud (n,b) -> begin
		   match mot with
		   | [] -> false (* A priori, on n'utilise jamais ce cas*)
		   | [t] -> b	 
		   | t::reste -> let l = (numero_lettre t)
				 in
				 member reste (Array.get n l)
		 end
		     
	       	     
let add ( m : mot) ( d : dico ) : dico =
  (* print_string "Entrée dans add \n" ;*)
  let m=m@[' '] in (* la dernière lettre n'est pas compté par la fonction, on rajoute un valeur factice, qui correspond au fait l'arbre a une profondeur (en terme de nombre de noeud donc en comptant la base) d'1 de plus que la taille maximal des mot qu'il définit. *)
  let rec sub_add ( m : mot) ( d : dico ) : dico =
  match d with
  | Feuille -> d
  | Noeud (n,b) -> 
     match m with
     |[] -> d
     |[t]-> Noeud (n,true)
     | t::reste -> let l  = (numero_lettre t) in
		   (* print_string "Appel sur un noeud fils\n" ;*)
		   let ncopy = Array.copy n in
		   let nmodifie = (sub_add reste n.(l)) in
		   let _ = (ncopy.(l)<-nmodifie) in (* sans copy utilisé une copy tout les sous arbre étaitsont mis à jour *)
		   Noeud (ncopy,b)in
  
  sub_add m d
		   

    
		     
let rec remove ( m : mot) ( d : dico ) : dico =
  match d with
  | Feuille -> failwith "Mot trop long"
  | Noeud (n,b) -> begin
		   match m with
		   | [] -> d
		   | [t] -> Noeud (n,false)
		   | t::reste -> let l  = (numero_lettre t) in
				 let nmodifie = (add reste (Array.get n l)) in
				 let modification = (Array.set n l nmodifie) in
				 Noeud (n,b)
				       
  end


     
(* renvoie la chaine de caractère en majuscule et sans accent (/!\ prévue seulement pour le dictionnaire du projet, pour d'autre utilisations il faudrais éventuellement rajouter des filtres pour d'autre caractère et/ou autre codages. *)
let uppercase_retirer_accents (s : string):string =
  let convert (c:string):string =
    match c with
(* en raison de la taille de leur codage, la valeur interne des caractère accentués (\ddd\ddd) est d'un manière ou d'une autre séparé en 2 lors de l'appel à String.make s.[i]. On filtre donc la première valeur qui est constante dans notre cas (\195) puis à l'itération suivante oest constante dans notre cas (\195) puis à l'itération suivante on traduit la valeur qui nous interesse.*)
    (* Pour le norme ISO/IEC 8859-1 *)
    |"\\224"->"A" (* à *)
    |"\\226"->"A" (* â *)
    |"\\232"->"E" (* è *)
    |"\\233"->"E" (* é *)
    |"\\234"->"E" (* ê *)
    |"\\238"->"I" (* î *)
    |"\\239"->"I" (* ï *)
    |"\\244"->"O" (* ô *)
    |"\\249"->"U" (* ù *)
    |"\\251"->"U" (* û *)
    |"\\231"->"C" (* ç *)
    |"\\195"->""
    |_->c in
  let news = ref (String.make 0 'c') in
  for i = 0 to (String.length s) - 1 do
    let c = convert (String.uppercase(String.escaped (String.make 1 s.[i]))) in
   (*print_string (c^" ");*)
    news := (!news ^ (c));
  done;
  !news
    
    
(* Permet de dÃ©terminer si une chaÃ®ne de caractÃ¨res est un mot en majuscules *)
let valide s =
  ((String.length s) <> 0) &&
    begin
      let ret = ref true in
      (* print_string "Debug 3" ;*)
      for i = 0 to (String.length s) - 1 do
	let c = Char.code s.[i] in
	(*print_string "Debug 7" ;*)
	ret := (!ret) && (c >= (Char.code 'A')) && (c <= (Char.code 'Z'))
      done;
      !ret
    end

(*let valide s =
  true*)

(* Permet de charger un dictionnaire en mettant tous les mots en majuscules *)
(* La fonction bug car elle ne s'arrete pas en fin de fichier *)

let of_file ( nomFichier : string ) : dico  =
  let flux = open_in nomFichier in
  let mondico = ref (dico_vide()) in
  try
    begin
      (*print_string "Debut de la lecture\n" ;*)
      while true do
	(*print_string "Chaine en majuscule \n" ;*)
	let l = uppercase_retirer_accents (input_line flux) in
	(*	print_string "Test Validité\n" ;*)
	if (valide l) then
	  begin
	    (* print_string ("Ajout de " ^ l ^ "\n") ;*)
	    mondico := add (explode l) (!mondico);
	  (*print_string "Ajout réalisé\n" ;*)
	  end;
      done;
      !mondico
    end
  with
    End_of_file -> !mondico



      
let to_list (d : dico ) : string list =
  let rec sub_to_list ( d : dico ) ( m : mot ) : string list =
    let l  = ref [] in
    match d with
    | Feuille -> !l
    | Noeud (n,b) -> 
       begin
	 (* print_string "Je parcours un noeud\n" ;*)
	 for i=0 to 25 do
	    (* print_string "Appel recursifs sur "; print_int i ; print_string "\n" ;*)
	    l := ( sub_to_list (Array.get n i) ( m @ [(lettre_numero i)] ) ) @ !l ;
	 (* print_string "retour d'appel recursifs\n" ;*)
	  (* print_string "Un noued de plus\n" ;*)
	 done ;
	 
	 if b=true then
	   (implode m)::!l
	 else
	   !l
       end

  in   sub_to_list d []


let rec afficher ( l : string list ) : unit =
  match l with
  | [] -> print_string "Fin de l'afficage \n" 
  | t::reste -> begin
		print_string (t^"\n")  ;
		afficher reste
	      end
		      
		     
	

