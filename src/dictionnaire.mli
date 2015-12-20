type dico =  Noeud of dico array * bool | Feuille
type lettre = char
type mot = lettre list

val explode : string -> char list
val dico_plein : dico
val dico_vide : dico
val member : mot -> dico -> bool
val add : mot -> dico -> dico
val remove : mot -> dico -> dico
val of_file : string -> dico
val to_list : dico -> string list
val afficher : string list -> unit
				
				


    
