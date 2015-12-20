let maPartie = ref (Jeu.creer_partie "Bob" "Toto" "../dico/dico_fr.txt") in
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
	
     
	
     
      
	
				   

				 
