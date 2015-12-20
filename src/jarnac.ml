(* A terminer *)

if Sys.argv.length = 1 then
  begin
    if Array.get argv 1 = "help" then
      print_string "Utilisation de Jarnac : \n\n
		    jarnac help -> afficher l'aide \n
		    jarnac nom1 nom2 dico -> Démarrer une partie de jarnac entre les joueurs nom1 et nom2 et le dictionnaire dico. L'argument dico est un chemin relatif vers un dictionnaire. Il est à noter que \"plein\" crée un dictionnaire acceptant tout les mots possédant de 3 à 9 lettres."
    else
      failwith "Commande incorrecte, taper jarnac help pour avoir de l'aide" 
  end 
else if Sys.argv.length = 3 then
 begin

let maPartie = ref (Jeu.creer_partie (Array.get Sys.argv 1) (Array.get Sys.argv 2) (Array.get Sys.argv 3)) in
    Jeu.put_partie (!maPartie) ;
    try
     
	while true  do
	  maPartie:=Jeu.jouerTour (!maPartie) ;
	  Jeu.put_partie (!maPartie) ;
	  maPartie:=Jeu.jouerTour (!maPartie) ;
	  Jeu.put_partie (!maPartie) ;
	done;
	
     
   with
     Jeu.Partie_termine -> Jeu.put_partie (!maPartie)
	
     
	
    end 
      
	
				   

				 
