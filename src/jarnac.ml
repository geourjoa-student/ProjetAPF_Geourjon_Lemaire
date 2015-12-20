let maPartie = ref (Jeu.creer_partie "Bob" "Toto" "../dico/dico-fr.txt") in
    try
      begin
	while true do
	  maPartie:=Jeu.jouerTourJ1 !maPartie;
	  put_partie maPartie ;
	  maPartie:=Jeu.jouerTourJ2 maPartie ;
	  put_partie maPartie ;
      done; 
      end
    with
      Partie_termine ->
      begin
	maPartie.termine <-true ;
	put_partie maPartie
      end
			  
      
	
				   

				 
