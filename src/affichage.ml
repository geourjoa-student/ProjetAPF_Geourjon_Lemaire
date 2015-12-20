let rec put_mot ( m : Type.mot) : unit =
  match m with
  | [] ->()
  | t::reste ->
     begin
       print_string "| ";
       print_char t;
       print_string " ";
       put_mot reste
     end

let put_main ( m : Type.mot ) : unit =
  put_mot m


let rec put_mots ( l : Type.mot list ) : unit =
  match l with
  | [] -> ()
  | t::reste ->
     begin
       put_mot t ;
       put_mots reste
     end


let put_joueur ( j : joueur ) : unit =
  begin
    print_string ("Joueur : " ^ j.nom ^ " Score : ") ; print_int j.score ;
    print_string "-------------------------------------\n";
    print_string "|   |   | 9 | 16| 25| 36| 49| 64| 81|\n";
    print_string "-------------------------------------\n";
    put_mots j.mots;
    print_string "-------------------------------------\n";
    put_main j.main ;
    print_string "-------------------------------------\n";
  end


let put_partie ( p : partie ) : unit =
  begin
    print_string " *** Jarnac *** \n" ;
    put_joueur p.joueur1 ;
    put_joueur p.joueur2
  end
