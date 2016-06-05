open Scanf;;
open Printf;;

type matrix = int list list

let lista1 = [1;4;6;2];;
let lista2 = [1;5;6;3];;
let lista3 = [2;4;3;5];;

let shuffle d =
	Random.self_init ();
	let nd = List.map (fun c -> (Random.bits (), c)) d in
	let sond = List.sort compare nd in
	List.map snd sond
;;

let matrizResposta = [(shuffle lista1);(shuffle lista2);(shuffle lista3)];;

let matrizInicial = [[0;0;0;0];
			   [0;0;0;0];
			   [0;0;0;0]]
;;


(* PRINT LIST INT *)
let rec print_list_int myList = match myList with
| [] -> print_endline ""
| head::body -> 
begin
	print_int head; 
	print_string " ";
	print_list_int body
end
;;

(* PRINT MATRIX INT*)
let rec print_matrix myMatrix = match myMatrix with
| [] -> print_string ""
| head::body ->
begin
	print_list_int head;
	print_matrix body
end
;;

(* PRINT LIST INT *)
let rec print_list_string myList = match myList with
	| [] -> print_endline ""
	| head::body -> 
	begin
		print_string head; 
		print_string " ";
		print_list_string body
	end
;;

(* PRINT MATRIX INT*)
let rec print_matrix_string myMatrix = match myMatrix with
	| [] -> print_string ""
	| head::body ->
	begin
		print_list_string head;
		print_matrix_string body
	end
;;

(* print_matrix_string(matStr);; *)

(* ECONTRA UM ELEMENTO NA LISTA *)
let rec find_list x lista =
	match lista with
	| [] -> -1
	| head :: body -> if x = head then head else find_list x body
;;

(* ENCONTRA UM ELEMENTO NA MATRIX *)
let rec find_matrix x matrix =
	match matrix with
	| [] -> -1
	| head::body -> if x = (find_list x head) then x else find_matrix x body
;;

(* ENCONTRA UM ELEMENTO NO VETOR PELA POSIÇÃO *)
let rec find x lista =
	match lista with
	| [] -> -1
	| head :: body -> if (x > 1) then find (x-1) body else head
;;

(* ENCONTRA UM ELEMENTO NA MATRIX DADA A POSIÇÃO X E Y *)
let rec find_element_matrix lin col matrix =
	match matrix with
	| [] -> -1
	| head::body -> if (lin > 1) then (find_element_matrix (lin-1) col body) else find col head
;;


let rec insert_at x n lista =
	match lista with
	| [] -> [x]
	| h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n-1) t
;;

let rec remove_at n lista= 
match lista with
| [] -> lista
| h :: t -> if n = 0 then t else h :: remove_at (n-1) t
;;



let ler_coordenada s =
	Printf.printf("Digite %s: ") s;
	read_int ()
;;	

(* Verifica se o jogador acertou a jogada *)
let verifica_acerto x1 y1 x2 y2 matrizResposta= 
let num1 = find_element_matrix x1 y1 matrizResposta in
let num2 = find_element_matrix x2 y2 matrizResposta in
if (num1 = num2) then num1 else -1
;;


(* Convert uma matriz de lista para uma matriz de Array *)
let convert ll = Array.of_list (List.map Array.of_list ll);;
(* Converte uma matriz de Array para uma matriz de List *)
let convertToList arr = Array.to_list (Array.map Array.to_list arr)
(* Troca um elemento da na matriz *)
let troca_elemento elemento lin col matrix_list =
	let arr = convert matrix_list in
	arr.(lin-1).(col-1) <- elemento;
	let m = convertToList arr in
	m
;;



let rec jogo contador fimJogo matrizResposta matrizJogo acertos =
	print_matrix matrizJogo;
	let x1 = ler_coordenada "uma coordenada para linha" in
	let y1 = ler_coordenada "uma coordenada para coluna" in
	Printf.printf("1º Elemento: %d: \n") (find_element_matrix x1 y1 matrizResposta);
	let x2 = ler_coordenada "outra coordenada para linha" in
	let y2 = ler_coordenada "outra coordenada para coluna" in
	Printf.printf("2º Elemento: %d: \n") (find_element_matrix x1 y1 matrizResposta);
	let acerto = verifica_acerto x1 y1 x2 y2 matrizResposta in
	if (acerto < 0) then print_string "\nErrou\n" else print_string "\nAcertou\n";
	if (acerto < 0) then (jogo (contador) fimJogo matrizResposta matrizJogo acertos)
	else let matrizJogo =  troca_elemento acerto x1 y1 matrizJogo in
		 let matrizJogo =  troca_elemento acerto x2 y2 matrizJogo in


	if ((contador < fimJogo) && (acerto > 0)) then (jogo (contador+1) fimJogo matrizResposta matrizJogo acertos) else print_string "Fim de jogo \n"
;;

let main =
print_string " === Jogo iniciado === \n";
print_matrix matrizResposta;
print_string "\n";
jogo 0 6 matrizResposta matrizInicial [];
