(*
 CONCERNANT LES TABLEAUX
*)

(** [asomme t] calcule la somme des éléments de t *)
let asomme t =
    let sum = ref 0 in
    for i=0 to Array.length t - 1 do
        sum := !sum + t.(i)
    done;
    !sum;;

(** [amaximum t] renvoie le maximum de t *)
let amaximum t =
    if Array.length t = 0 then failwith "amaximum : Try to get a maximum among a set of 0 elements";
    let maxi = ref t.(0) in
    for i=0 to Array.length t - 1 do
        if !maxi < t.(i) then maxi := t.(i);
    done;
    !maxi;;

(** [aminimum t] renvoie le minimum de t *)
let aminimum t =
    if Array.length t = 0 then failwith "aminimum : Try to get a minimum among a set of 0 elements"
    let mini = ref t.(0) in
    for i=0 to Array.length t - 1 do
        if !mini > t.(i) then mini := t.(i);
    done;
    !mini;;

(** [list_of_array t] convertit le tableau t en liste *)
let list_of_array t =
    let rec aux n =
        if n <= 0 then []
        else t.(n)::(aux (n-1)) in
    aux (Array.length t - 1)

(** [acroissant t] renvoie true si t est croissante, et false sinon *)
let acroissant t =
    let i = ref 0 in
    let len = Array.length t in
    while !i < len - 2 && t.(!i) <= t.(!i + 1) then
        incr i;
    done;
    !i = len - 1;;

(** [max_local t] renvoie un maximum local propre à t *)
let max_local t =
    let max_local t = 
    let n = Array.length t in
    let rec aux i j =
        let m = (i + j)/2 in
        if (m = 0 || t.(m) >= t.(m-1)) && (m = n - 1 || t.(m) >= t.(m+1))
        then m
        else if t.(m) < t.(m - 1) then aux i (m - 1)
        else aux (m + 1) j in
    aux 0 (n + 1);;

(** [tranche_max t] renvoie la somme maximum d'éléments consecutifs de t *)
let tranche_max t =
    let m = ref t.(0) in
    let m_cur = ref t.(0) in
    for i = 1 to Array.length t - 1 do
        m_cur := max (!m_cur + t.(i)) t.(i);
        m := max !m !m_cur
    done;
    !m;;

(** [inversions t] renvoie le nombres d'e couples inversibles de t *)
let inversions t =
    let res = ref 0 in
    let n = Array.length t in
    for i=0 to n - 1 do 
        for j=i+1 to n - 1 do
            if t.(i) > t.(j) then res := !res + 1
        done
    done;
    !res;;

(** [areverse t] renverse t *)
let areverse t =
    let len = Array.length in
    let ret = Array.make len (t.(0)) in
    for i=0 to len do
        ret.(i) = t.(len - 1 - i)
    done;
    ret;

(** [aresearch e t] renvoie true si e appartient à t, et false sinon *)
let rec aresearch e t =
    let i = ref 0 in
    while !i < Array.length && t.(!i) <> e do
        i := !i + 1
    done;
    






(*
 CONCERNANT LES LISTES
*)

(** [lsomme l] calcule la somme des éléments de l *)
let rec lsomme = function
    | [] -> 0
    | e::q -> e + (lsomme q);;

(** [lmaximum l] renvoie le maximum de l *)
let rec lmaximum = function
    | [] -> failwith "lmaximum : Try to get a maximum among a set of 0 elements"
    | [e] -> e
    | e::e2::q -> let prev_max = lmaximum (e2::q) in
                    if e > prev_max then e
                    else prev_max;;

(** [lminimum ] renvoie le minimum de t *)
let rec lminimum = function
    | [] -> failwith "lminimum : Try to get a minimum among a set of 0 elements"
    | [e] -> e
    | e::e2::q -> let prev_min = lminimum (e2::q) in
                    if e < prev_min then e
                    else prev_min;;

let rec lcroissant = function
    | e::e2::q -> e <= e2 && croissant (e2::q)
    | _ -> true;;

let rec lreverse = function
    | [] -> []
    | e::q -> (reverse q) @ [e];;

let rec lresearch e l =
    | [] -> false
    | e2::q -> e2 = e || research e q;;





(*
 DIFFÉRENTS TRIS
*)

(** [split l] sépare l en deux listes de tailles égales *)
let rec split = function
    | [] -> [], []
    | [e] -> [e], []
    | e1::e2::q -> let q1, q2 = split q in e1::q1, e2::q2;;

(** [fusion l1 l2] fusionne l1 et l2 de manière à ce que la résultante soit triée et croissante *)
let rec fusion l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | e1::q1, e2::q2 when e1 < e2 -> e1::fusion q1 l2
    | e1::q1, e2::q2 -> e2::fusion l1 q2;;

(** [tri_fusion l] trie l en complexité O(n*ln(n)) *)
let rec tri_fusion = function
    | [] -> []
    | [e] -> [e]
    | l -> let l1, l2 = split l in fusion (tri_fusion l1) (tri_fusion l2);;  




(** [concat l1 l2] concatène l1 et l2 *)
let rec concat l1 l2 = match l1 with
    | [] -> l2
    | e::q -> e::(concat q l2);;
    
(** [partition p l] sépare l en deux parties : la première contenant les éléments strictement inférieurs à p, et les autres *)
let rec partition p l = match l with
    | [] -> ([], [])
    | e::q -> let l1, l2 = (partition p q) in
                if e < p then ((e::l1), l2)
                else (l1, (e::l2));;

(** [quicksort l] trie en complexité variable de O(n*ln(n))) à O(n^n) selon les conditions initiales *)
let rec quicksort l = match l with
    | [] -> []
    | [e] -> [e]
    | e::q -> let l1, l2 = partition e q in
        concat (quicksort l1) (e::quicksort l2);;
