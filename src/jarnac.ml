let maPartie = ref (Jeu.creer_partie "Bob" "Toto" "../dico/dico_fr.txt") in

      begin
	while true  do
	  maPartie:=Jeu.jouerTourJ1 (!maPartie);
	  Affichage.put_partie (!maPartie) ;
	  maPartie:=Jeu.jouerTourJ2 (!maPartie) ;
	  Affichage.put_partie (!maPartie) ;
	done;
	Affichage.put_partie (!maPartie)
      end
   
     
	
     
      
	
				   

				 
