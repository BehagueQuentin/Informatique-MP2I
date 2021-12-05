let rec insert l tuple =
   let a, b, c = tuple in
      match l with
      | [] -> [tuple]
      | (e, f, g) :: t when f < b -> (e,f,g) :: (insert t tuple)
      | _ -> tuple :: l;;

let rec tri l =
   match l with
   | [] -> []
   | tuple :: t -> insert (tri t) tuple;;

(*******************************************************************************)

let rec fonction l_ord j d_j acc =
   match j, l_ord with
   | _, [] -> failwith "Erreur de Segmentation : j doit être inférieur au nombre de tâches"
   | 1, _ -> 0
   | _, (a, b, c) :: t when (b <= d_j) -> max (fonction t (j - 1) d_j (acc + 1)) acc
   | _, (a, b, c) :: t -> fonction t (j - 1) d_j (acc + 1) ;;

let appel_f l_ord j d_j = fonction l_ord j d_j 1;;

(*******************************************************************************)

let appel_create_tab l =
	let ltri = tri l in
	let rec create_tab l acc =
   		match l with
   		| [] -> []
   		| (a, b, c) :: t -> (appel_f ltri acc a)::(create_tab t (acc+1)) in
 	create_tab ltri 1;;

let l = [(0,4,50);(2,7,32);(10,20,45);(9,11,20);(4,12,36);(18,19,12)];;
tri l;;

appel_create_tab (tri l);;


let rec soluce liste_tache acc =
   match liste_tache with
   | [] -> acc
   | (a,b,c)::t -> max (soluce t acc) (soluce (decoupe l) (acc+))
   ;;
