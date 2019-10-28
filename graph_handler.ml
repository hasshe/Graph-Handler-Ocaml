(*
Student in computer engineering at the University of Gävle

Author: Hassan Sheikha
E-mail: sheikha.hassan.rb@gmail.com
Date: 2019-05-28

Reads a text file with inputs to generate a graph.
For this generated graph, the edges are counted,
the sides, as well as the graph gets printed.
Furthermore, cycles for a node are found and printed and
paths are found and printed between two nodes. 
*)

open List;;

type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list };;

(*Reads a file character by character*)
let input_char_opt ic =
  try Some (input_char ic)
  with End_of_file -> None
;;

(*Reads a file line by line*)
let input_line_opt ics =
  try Some (input_line ics)
  with End_of_file -> None
;;

(*Reads all lines in a file*)
let read_lines ic =
  let rec aux acc = 
    match input_char_opt ic with 
    |Some line ->  aux (line :: acc)
    |None -> (List.rev acc)
  in 
  aux []
;;

(*Sorts a list in ascending order*)
let rec sort lst =
  match lst with
    [] -> []
  | head :: tail -> insert head (sort tail)
and insert elt lst =
  match lst with
    [] -> [elt]
  | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail
;;

(*Removes duplicate elements*)
let remove_element e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []
;;

(*Finds duplicate elements*)
let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_element x xs) (x::acc)
  in go l []
;;

(*Gets all nodes in a file and removes*)
let get_node filename =
  let ic = open_in filename in 
  let lines = read_lines ic in 
  let sorted = sort lines in
  let fin = remove_duplicates sorted in
  close_in ic;
  (fin)
;;

(*Reads all lines in the file*)
let lines_of_file filename =
  let ic = open_in filename in 
  let lines = read_lines ic in 
  close_in ic;
  (lines)
;;

let read_lines_left ics =
  let rec aux acc = 
    match input_line_opt ics with 
    |Some line ->  aux (line.[0] :: acc)
    |None -> (List.rev acc)
  in 
  aux []
;;

(*Reads all lines in the file*)
let lines_of_file_left filename =
  let ic = open_in filename in 
  let lines = read_lines_left ic in 
  close_in ic;
  (lines)
;;

(*Reads all lines in a file and makes a list of all elements at index 1*)
let read_lines_right ics =
  let rec aux acc = 
    match input_line_opt ics with 
    |Some line ->  aux (line.[1] :: acc)
    |None -> (List.rev acc)
  in 
  aux []
;;

(*Reads all lines in the file*)
let lines_of_file_right filename =
  let ic = open_in filename in 
  let lines = read_lines_right ic in 
  close_in ic;
  (lines)
;;

(*Encodes a list to compress sequences of the same element*)
let encoder list =
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> (count+1, x) :: acc
    | a :: (b :: _ as t) -> 
      if a = b 
      then aux (count + 1) acc t
      else aux 0 ((count+1,a) :: acc) t in
  List.rev (aux 0 [] list)
;;

(*Reads lines from file and generates a list of all elements that are not = '-'*)
let get_edges ic =
  let rec aux acc = 
    match input_char_opt ic with 
    |Some line -> if line != '-' then aux (line :: acc) else aux acc
    |None -> (List.rev acc)
  in 
  aux []
;;

(*Encodes a list to compress sequences of the same element*)
let encode filename =
  let ic = open_in filename in 
  let lines = get_edges ic in 
  let sorted = sort lines in
  let final = encoder sorted in
  close_in ic;
  (final)
;;

(*Prints a polymorph list*)
let print_list f lst =
  let rec print_elements = 
    function
    | [] -> ()
    | h::t -> f h; 
      print_char ':'; 
      print_elements t
  in
  print_elements lst
;;

(*My graph generated from a text file*)
let my_graph =
  { nodes = get_node "graph.txt";
    edges = List.combine (lines_of_file_left "graph.txt") (lines_of_file_right "graph.txt") }
;;

(* Returns all neighbors satisfying the condition. *)
let neighbors g a cond =
  let edge l (b,c) = if b = a && cond c then c :: l
    else if c = a && cond b then b :: l
    else l in
  List.fold_left edge [] g.edges
;;
(*Lists a path from node x  to node y*)
let rec list_path g a to_path = match to_path with
  | [] -> assert false
  | a' :: _ ->
    if a' = a then [to_path]
    else
      let n = neighbors g a' (fun c -> not(List.mem c to_path)) in
      List.concat(List.map (fun c -> list_path g a (c :: to_path)) n)
;;
(*Compares two nodes for path generation*)
let paths g a b =
  assert(a <> b);
  list_path g a [b]
;;
(*Takes a node and generates all cycles for that node*)
let cycles g a =
  let n = neighbors g a (fun _ -> true) in
  let p = List.concat(List.map (fun c -> list_path g a [c]) n) in
  List.map (fun p -> p @ [a]) p
;;

let found_cycle = (List.flatten (cycles my_graph 'A'));;
print_string "Found Cycles: ";
List.iter print_char found_cycle;;
print_newline ();;

let found_path = (List.flatten (paths my_graph 'A' 'B'));;
print_newline ();
print_string "Found Path: ";
List.iter print_char found_path;;
print_newline ();
;;

let () =
  print_newline ();
  print_string "Amount of Edges: ";
  print_int (List.length my_graph.nodes - 1);
  print_newline ();;

let () =
  print_newline ();
  print_string "Amount of Sides: ";
  print_int (List.length my_graph.edges);
  print_newline ();;

let () =
  let lines = lines_of_file "graph.txt" in 
  print_newline();
  print_string "The Graph: ";
  print_newline();
  List.iter print_char lines;
  print_newline();
  print_newline();
;;

let () =
  let lines = encode "graph.txt" in 
  print_endline "The Valens For Each Edge: ";
  print_list (fun (a, b) -> print_int a; 

               print_char  b;) lines;
  print_newline ()
;;