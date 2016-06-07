open Printf;;
open Scanf;;
(* let leu = Scanf.bscanf sb " %d" (fun a -> a) in *)
let l = [ 2; 8; 5;];;
let mat = [[[1,2,3],[4,5,6]],[[7,8,9]]];;
let tam = List.length l;;

let rec lista_aleatoria = function 
  |0 -> []
  |n -> (Random.int 10000)::(lista_aleatoria (n-1))
;;

let rec lista_aleatoria2 m = 
  match m with  
    |0 -> []
    |n -> (Random.int 25)::(lista_aleatoria2 (n-1))
;;

let rec lista_aleatoria3 m = 
  if m=0 then []
  else (Random.int 25)::(lista_aleatoria3 (m-1))
;;

let l = lista_aleatoria3 5;;

(* let () = List.length x (printf "%d ") *)

(* let randomize l =
  let random_cmp x y =
    if x = y then 0
    else if Random.bool() then 1 else -1
  	in
	List.sort random_cmp l

# randomize l; *)
let () = List.iter (printf "%d ") l;;
Printf.printf "Tamanho da lista: %d\n" tam;;