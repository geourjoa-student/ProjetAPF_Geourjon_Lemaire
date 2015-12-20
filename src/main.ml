Printf.printf "Bienvenue dans le jeu de jarnac \n"

  (* let monDico = Dictionnaire.of_file "dico_fr.txt";;

   *)

let rec longueur l = match l with
  | [] -> 0
  | x::reste -> 1 + (longueur reste);;
	      
let monDico = Dictionnaire.dico_vide 



let monDico = Dictionnaire.add (Dictionnaire.explode "M") monDico 
			       
			       
let monDico = Dictionnaire.add (Dictionnaire.explode "T") monDico
 			       
let monDico = Dictionnaire.add (Dictionnaire.explode "B") monDico 
			       
let lesMots = Dictionnaire.to_list monDico

let _ = print_int (longueur lesMots)
 
    let _ =   Dictionnaire.afficher lesMots
 
  
