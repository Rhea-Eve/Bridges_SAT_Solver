
(*types*)
type pos = 
Pos of int * int

type island = 
  Island of int * pos

type bridge =
  Con of pos * pos * bool (*note: bool is t/f for f is this is the first coneection. t is it is the second*)

type or_statment =
  Or of (bridge * bool) list (*note, t/f is if it is true or not*)
 (* | Set_or of ( island list * (bridge * bool) list) (*note: this is for the sets for the conectivity function*) *)


let rec string_of_pos (Pos(x, y)) =
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let string_of_island (Island(id, pos)) =
  "Island " ^ string_of_int id ^ " at " ^ string_of_pos pos

let rec string_of_island_list lst =
  match lst with
  | [] -> ""
  | island::tl ->
    string_of_island island ^ "; " ^ string_of_island_list tl

let string_of_bridge (Con(pos1, pos2, is_connected)) =
  "Bridge from " ^ string_of_pos pos1 ^ " to " ^ string_of_pos pos2 ^ " (" ^ string_of_bool is_connected ^ ")"

let rec string_of_bridge_list lst =
  match lst with
  | [] -> ""
  | bridge::tl ->
    string_of_bridge bridge ^ "; " ^ string_of_bridge_list tl

let rec string_of_or_list bridges =
  match bridges with
  | [] -> ""
  | (bridge, is_true)::tl ->
    string_of_bridge bridge ^ " is " ^ string_of_bool is_true ^ "; " ^ string_of_or_list tl

let string_of_or_statement or_stmt =
  match or_stmt with
  | Or(bridges) ->
    "Or: " ^ string_of_or_list bridges

let rec string_of_or_statement_list or_stmt_list =
  match or_stmt_list with
  | [] -> ""
  | or_stmt::tl ->
    string_of_or_statement or_stmt ^ "; " ^ string_of_or_statement_list tl



(*TOKENIZER/PARSER string input --> (dimensions, list of islands)*)

let tokenizer string =
  let remove_first_char (str: string) =
    let help i (c: char) =
      if i = 0 then ' ' else c in
    String.mapi help str in

  let get_first_char str =
    if String.length str > 0 then
       (String.get str 0)
    else
      failwith "Empty string" in

  let new_string = String.trim string in (*removes white space*)

  (*let n = int_of_char (String.get new_string 0) in *)

  let first_char = get_first_char new_string in 
  let new_new_string = remove_first_char new_string in 

  let d = int_of_char first_char - 48 in (*dimensions for the mod*)

  let tokenizer_process_char (i, acc) c =
    let col =  i mod (d) in
    let row =  ((i-col) / d) in
    match c with
      | '1' .. '9' -> (i +1, Island (int_of_char c - 48, Pos (col, row))::acc )
      | '.' -> (i +1, acc )
      | ' '  -> (i, acc)
      | '\t' -> (i, acc)
      |  '\n' -> (i, acc)
      | '\r' -> (i, acc)
      | _ -> failwith "Unexpected character"
       in

      let (_, list) = String.fold_left tokenizer_process_char (0, []) new_new_string in 
      (d, List.rev list)

(*test of tokenizer*)
let tokenizer_test ()= 
  let string = 
"7 
1.3..2. 
....... 
4.5...2 
....... 
..6..2. 
2......
..2.1.2" in 
 let (i, islands) = (tokenizer string) in 
  print_string (string_of_int i ^ string_of_island_list (islands))

(*make a list/array of possible connections
* this is of size ((number of nodes) C 2*2 
* used to determine the numbers of the vars
O make into map later maybe?*)

(*updated to only unclude possible bridges. 
   you only look at the left and the down directions 
* no repeates
* also no wrong rows/colums
* also no crossing over islands

O to work, the islands must be in order (can easily fix this 
by going a more general search if needed)

*)



let make_possible_bridge_list island_list = 

  (*gets the first island to the right, and makes the two bridges to this island*)
  let get_right (Island (_, Pos (x1, y1))) islands = 
    let rec help islands_to_check =
      match islands_to_check with (Island (_, Pos (x2, y2)))::rest ->
        if ((y1 = y2) && (x1 < x2))
          then [(Con (Pos (x1, y1), Pos (x2, y2), false)) ;  (Con (Pos (x1, y1), Pos (x2, y2), true))] 
        else help rest
      | [] -> [] in 
    help islands in 

  (*gets the first island down, and makes the two bridges to this island*)
  let get_down (Island (_, Pos (x1, y1))) islands = 
    let rec help islands_to_check =
      match islands_to_check with (Island (_, Pos (x2, y2)))::rest ->
        if ((x1 = x2) && (y1 < y2))
          then [(Con (Pos (x1, y1), Pos (x2, y2), false)) ;  (Con (Pos (x1, y1), Pos (x2, y2), true))] 
        else help rest
      | [] -> [] in 
    help islands in 

    (* test for the above two functions
     print_string (string_of_bridge_list get_down (Island (2, Pos (0, 1))) [(Island (2, Pos (0, 2)))]) *)

    let rec get_down_and_right i islands  =
     (get_right i islands) @ (get_down i islands) in 

    let rec all_possible islands = 
      match islands with i::rest ->  (get_down_and_right i rest ) @ all_possible rest 
      | [] -> [] in 

    all_possible island_list


(*test of make_possible_bridge_list_test*)
let make_possible_bridge_list_test () = 
    let string = 
      "7 
      ....... 
      ....... 
      ...1..1 
      ...1.12 
      ....... 
      .......
      ......."  in 
    let (i, islands) = (tokenizer string) in 
    print_string (string_of_bridge_list (make_possible_bridge_list islands))


(*add cnf for no overlapping bridges

*writes cnf (or_statments) which chech to make sure that nothing is overlaping 
   
O idea for obtimization: better deal with the 2 possible 
bridges... like do that after all the processing*)


let bridge_overlapping_check bridge_list = 

  (*note this only checks for overlap when the first bridge is horizontal*)
  let check_for_overlap (Con (Pos (x1, y1), Pos (x2, y2), t_f_1)) (Con (Pos (x3, y3), Pos (x4, y4), t_f_2)) = 
    let check_for_first_con_horiz = (y1 = y2) in 
    let check_for_second_con_vert = (x3 = x4) in 

    let check_if_first_con_y_is_between_second_con = ( ((y3 > y1) && (y1> y4)) || ((y3 < y1) && (y1 < y4))) in 
    let check_if_second_con_x_is_between_first_con = (((x1 > x3) && (x3 > x2))|| ((x1 < x3) && (x3 < x2))) in 

    if check_for_first_con_horiz && check_for_second_con_vert && check_if_first_con_y_is_between_second_con && check_if_second_con_x_is_between_first_con
    then [Or [((Con (Pos (x1, y1), Pos (x2, y2), t_f_1)), false) ; (((Con (Pos (x3, y3), Pos (x4, y4), t_f_2))), false)]]
    (*if they overlap then we need not one, or not the other*)
    else [] in

  let rec search to_check1 to_check2 = 
    match to_check2 with b::rest2 ->
      (match to_check1 with a::rest1 -> ((search to_check1 rest2) @ (check_for_overlap a b))
      | [] -> [] )
    | [] -> 
       (match to_check1 with a::rest1 -> (search rest1 bridge_list)
      | [] -> [] ) in 

    search bridge_list bridge_list


  (*a test for bridge overlap check *)
let test_bridge_overlapping_check () = 
      let string = 
"7 
1.3..2. 
....... 
4.5...2 
....... 
..6..2. 
2......
..2.1.2" in 
    let (i, islands) = (tokenizer string) in 
    let possible_bridges = make_possible_bridge_list islands in 
    let bridge_overlapping_check_cnfs = bridge_overlapping_check possible_bridges in 
    (*print_string (string_of_bridge_list possible_bridges)*)

    print_string (string_of_or_statement_list bridge_overlapping_check_cnfs)



(*add cnfs to check that each island is
  less than max number of the connections

* check that you have not more than the number of conections
*check that you have not less than the number of connections
  *)


let  max_conect bridge_list island_list = 

  (*get a list of bridges with pos*)
  let rec search_for_bridges (pos) bridges = 
    match bridges with 
    (Con (pos1, pos2, t_f))::rest -> 
     if ((pos = pos1) || (pos = pos2))  then (Con (pos1, pos2, t_f))::(search_for_bridges (pos) rest) 
      else (search_for_bridges (pos) rest ) 
    |  [] -> [] in 

  (*returns all the sublits of the given pridges in Or form  *)
  let rec sublists bridges = 
    match bridges with 
    | []    -> [Or []]
    | x::xs -> 
    let ls = sublists xs in
    (List.map (fun (Or l) -> Or ((x, false)::l)) ls) @ ls in (*you want at least ONE of the bridges in number_of_cons + 1 to be FALSE*)

  (*number_of_cons is the number of bridges this island can have*)
  let deal_with_island (Island (number_of_cons, position)) =  
    let bridges = search_for_bridges position bridge_list in
    let sublists = sublists bridges in 
    let cnfs = List.filter (fun (Or x) -> ((List.length x) = (number_of_cons + 1))) sublists in (*you want at least ONE of the bridges in number_of_cons + 1 to be FALSE*)
    cnfs in 
  
  let rec help island_list = 
    match island_list with i::rest -> deal_with_island i @ help rest
    | [] -> []  in 

  help island_list



let  min_conect bridge_list island_list = 


  (*get a list of bridges with pos*)
  let rec search_for_bridges (pos) bridges = 
    match bridges with 
    (Con (pos1, pos2, t_f))::rest -> 
     if ((pos = pos1) || (pos = pos2))  then (Con (pos1, pos2, t_f))::(search_for_bridges (pos) rest) 
      else (search_for_bridges (pos) rest ) 
    |  [] -> [] in 

  (*returns all the sublists of the given pridges in Or form  *)
  let rec sublists bridges = 
    match bridges with 
    | []    -> [Or []]
    | x::xs -> 
    let ls = sublists xs in
    (List.map (fun (Or l) -> Or ((x, true)::l)) ls) @ ls in (*you want at least ONE of the bridges in num_bridges - number_of_cons + 1 to be TRUE*)

  (*number_of_cons is the number of bridges this island can have*)
  let deal_with_island (Island (number_of_cons, position)) =  
    let bridges = search_for_bridges position bridge_list in
    let num_bridges = List.length bridges in 
    let sublists = sublists bridges in 
    let cnfs = List.filter (fun (Or x) -> ((List.length x) = ((num_bridges - number_of_cons) + 1))) sublists in (*you want at least ONE of the bridges in num_bridges - number_of_cons + 1 to be TRUE*)
    cnfs in 
  
  let rec help island_list = 
    match island_list with i::rest -> deal_with_island i @ help rest
    | [] -> []  in 

  help island_list



let max_test () = 
    let string = 
"7 
1.3..2. 
....... 
4.5...2 
....... 
..6..2. 
2......
..2.1.2" in 
  let (i, islands) = (tokenizer string) in 
  let possible_bridges = make_possible_bridge_list islands in 
  let max_check = max_conect possible_bridges islands in 
  (*print_string (string_of_bridge_list possible_bridges)*)

  print_string (string_of_or_statement_list max_check)


let min_test () = 
    let string = 
"7 
1.3..2. 
....... 
4.5...2 
....... 
..6..2. 
2......
..2.1.2" in 
  let (i, islands) = (tokenizer string) in 
  let possible_bridges = make_possible_bridge_list islands in 
  let min_check = min_conect possible_bridges islands in 
  (*print_string (string_of_bridge_list possible_bridges)*)

  print_string (string_of_or_statement_list min_check)

  
(*make cnfs to check for connected-ness*)

let connected_sat_statments (islands: island list) (bridges: bridge list) = 
  (*returns all the sublists of the given island_list*)
  let rec sublists island_list = 
    match island_list with 
    | []    -> [[]]
    | x::xs -> 
    let ls = sublists xs in
    (List.map (fun (l) ->  (x::l)) ls) @ ls in 

  let divide_and_round_down n =
    if n mod 2 = 0 then
      n / 2
    else
      (n  - 1)/2 in 

  let sublists = sublists islands in 
  let max_size = divide_and_round_down (List.length islands) in 
  let sublists_to_use = List.filter (fun (x) -> (List.length x) <= max_size && (List.length x) >= 1)  sublists in 

  let opp_sublist list =
    List.filter (fun x -> not (List.mem x list) ) islands in

  (*all bridges form a to b*)
  let rec all_bridges_from_a_to_b alist blist blist_full  = 
      match alist with Island (_, a)::rest1 -> (
        match blist with Island (_,b)::rest2 -> (
          if (List.mem (Con (a, b, true)) bridges ) then 
            (Con (a, b, true), true):: (Con (a, b, false), true) :: all_bridges_from_a_to_b alist rest2 blist_full
            
          else if (List.mem (Con (b, a, true)) bridges ) then 
            (Con (b, a, true), true):: (Con (b, a, false), true) :: all_bridges_from_a_to_b alist rest2 blist_full
            
          else all_bridges_from_a_to_b alist rest2 blist_full)
        |[] -> all_bridges_from_a_to_b rest1 blist_full blist_full)
        | [] -> ([]) in
    
  let a_bridge = match bridges with a::rest -> a | _ -> failwith "why are there no bridges" in 
  let rec make_cnfs sublists_to_use = 
    match sublists_to_use with 
    a::rest -> 
      let b = opp_sublist a in 
      let bridges_a_to_b = all_bridges_from_a_to_b a b b in 
      if bridges_a_to_b = [] then ([Or [(a_bridge, true)]; Or [(a_bridge, false)]]) (* we are throwing away our claim*) (*(print_string (string_of_island_list a ^ "AND " ^ string_of_island_list b ^ "/n" ^ string_of_bridge_list bridges ) ;*)
      else Or (all_bridges_from_a_to_b a b b) :: make_cnfs rest
    | [] -> [] in

    make_cnfs sublists_to_use

    
    let connected_test () = 
      let string = 
  "7 
  1.3..2. 
  ....... 
  4.5...2 
  ....... 
  ..6..2. 
  2......
  ..2.1.2" in 
    let (i, islands) = (tokenizer string) in 
    let possible_bridges = make_possible_bridge_list islands in 
    let connected_sat_statments_check = connected_sat_statments islands possible_bridges in
  
    print_string (string_of_or_statement_list connected_sat_statments_check)


(*turn sat readable. each var is the index of the list + 1 (so that there are no 0s) *)
let rec turn_sat_readable or_statments bridge_list= 

  let find_index lst element =
    match List.fold_left (fun (i, acc) x -> if x = element then (i + 1, Some i) else (i + 1, acc)) (0, None) lst with
    | (_, Some index) -> index
    | _ -> failwith "Not in list of bridges..." in

  let rec deal_with_statment (Or lst)  = 
     let rec hlp lst =
      match lst with (a, t_f)::rest -> 
        if t_f then string_of_int ((find_index bridge_list a ) + 1) ^ " " ^ hlp rest 
        else string_of_int (-((find_index bridge_list a) + 1)) ^ " " ^ hlp rest 
      | [] -> "0" in 
      "\n" ^ hlp lst in 
  
  let rec deal_with_list list =
    match list with 
    a::rest -> deal_with_statment a ^ deal_with_list rest 
    | [] -> "" in 

  let make_header =
    "p cnf " ^ string_of_int (List.length bridge_list) ^ " " ^ (string_of_int (List.length or_statments)) in 

   make_header^(deal_with_list or_statments)



let turn_sat_readable_test () = 
      let string = 
  "7 
  1.3..2. 
  ....... 
  4.5...2 
  ....... 
  ..6..2. 
  2......
  ..2.1.2" in 
    let (i, islands) = (tokenizer string) in 
    let possible_bridges = make_possible_bridge_list islands in 

    let max_check = max_conect possible_bridges islands in 
    let min_check = min_conect possible_bridges islands in 


    let connected_sat_statments_check = connected_sat_statments islands possible_bridges in

    let all_cnfs = (max_check @ min_check @ connected_sat_statments_check) in

    let turn_sat_readable_check = turn_sat_readable all_cnfs possible_bridges in 
  
    print_string (turn_sat_readable_check)
  

(*dimascus and actaully solving and stuff*)

(*turing sat out-put list --> bridge list
   
Con of pos * pos * bool*)
let rec sat_out_put_to_bridge (bridge_list: bridge list) sat_list =
  match sat_list with a::rest ->
    if (a > 0 && a < (List.length bridge_list + 1))then (List.nth bridge_list (a-1)):: (sat_out_put_to_bridge bridge_list rest) (*search list*)
    else sat_out_put_to_bridge bridge_list rest
  | _ -> [] 

(* Prints a puzzle solution s.t. lines (e.g. |=-) represent
   connections, numbers represent islands, dots represent empty
   cells and spaces between cells represent the absence of
   connections *)
let print_solution n islands bridges =
  let rec conn_horiz brdg i j = match brdg with
    (Con (Pos (x1, y1), Pos (x2, y2), t))::bs ->
      if y1 = y2 && y1 = i && ((x1 <= j && x2 > j) || (x2 <= j && x1 > j)) then
        (* found it, now let's figure it if it's a double-bridge *)
        (if (List.exists (fun x -> x = (Con (Pos (x1, y1), Pos (x2, y2), (not t)))) brdg) then "="
         else "-")
      else conn_horiz bs i j
  | [] -> " " in
  let rec conn_vert brdg i j = match brdg with
    (Con (Pos (x1, y1), Pos (x2, y2), t))::bs ->
      if x1 = x2 && x1 = j && ((y1 <= i && y2 > i) || (y2 <= i && y1 > i)) then
        (* found it, now let's figure it if it's a double-bridge *)
        (if (List.exists (fun x -> x = (Con (Pos (x1, y1), Pos (x2, y2), (not t)))) brdg) then "‖"
         else "|")
      else (conn_vert bs i j)
  | [] -> " " in
  let isl i j = match List.find_opt (fun (Island (_, p)) -> p = (Pos (j, i))) islands with
    Some (Island (i, _)) -> String.make 1 (Char.chr (48 + i))
  | None -> (match conn_horiz bridges i j with " " ->
            (match conn_vert bridges i j with " " -> "." | x -> x) | x -> x) in
  let part1 i j = (isl i j) ^
                (if j = n - 1 then "\n"
                else conn_horiz bridges i j) in
  let part2 i j = (conn_vert bridges i j) ^ " " in
  let arr = List.init n (fun i -> (List.init n (part1 i)) @ (if i = n - 1 then [] else (List.init n (part2 i)) @ ["\n"])) in
  List.fold_left (fun acc a -> (acc ^ (List.fold_left (fun b c -> b ^ c) "" a))) "" arr
        
  type minisatResult =
  Sat of (int list)
| Indet
| Unsat

exception SolverError of string

(* write into file, invoke the solver, and read its output *)
let invoke_minisat text =
  let parseList x =
    List.map int_of_string (List.filter (fun x -> x <> "") (String.split_on_char ' ' x)) in
  let inp = Filename.temp_file "sat" "inp" in
  let outp = Filename.temp_file "sat" "outp" in
  let inpf = open_out inp in
  Printf.fprintf inpf "%s" text;
  close_out inpf;
  let _ = Sys.command ("minisat " ^ inp ^ " " ^ outp) in
  let outpf = open_in outp in
  let first = input_line outpf in
  let second = (match input_line outpf with
    x -> x | exception End_of_file -> "") in
  close_in outpf;
  match first with
    "SAT" -> Sat (parseList second)
  | "INDET" -> Indet
  | "UNSAT" -> Unsat
  | _ -> raise (SolverError ("unknown status \"" ^ first ^ "\""))

type solverResult =
  SolvedSat of (bridge list)
| NotSAT
| NotSolved


(*fix this!*)

let rec print_bridge_list bridge_list = 
  match bridge_list with (Con (Pos (x1_check, y1_check), Pos (x2_check, y2_check), t_f_check))::rest -> "BRIDGE: [ (" ^ string_of_int x1_check ^ " , " ^ string_of_int y1_check ^ " ) (" ^ string_of_int x2_check ^  " , " ^ string_of_int y2_check ^ " ) , " ^ string_of_bool t_f_check ^ " ]" ^ (print_bridge_list rest)
  | [] -> "" 

let solve islands =
  (*let (i, islands) = (tokenizer string) in *) (*note: we feed the string in *)
  let possible_bridges = make_possible_bridge_list islands in 

  let max_check = max_conect possible_bridges islands in 
  let min_check = min_conect possible_bridges islands in 

  let bridge_overlapping_check_cnfs = bridge_overlapping_check possible_bridges in 



  let connected_sat_statments_check = connected_sat_statments islands possible_bridges in

  let all_cnfs = (max_check @ min_check @ connected_sat_statments_check @ bridge_overlapping_check_cnfs) in
   
  let turned_sat_readable = turn_sat_readable all_cnfs possible_bridges in 

(*
  print_string turned_sat_readable ;
  print_string (string_of_bridge_list possible_bridges);*)


  match invoke_minisat turned_sat_readable with
    Sat x -> SolvedSat (sat_out_put_to_bridge possible_bridges x)
  | Indet -> NotSolved
  | Unsat -> NotSAT


let solve_test () = 
  let string = 
"5
1...1
.....
.....
.....
1...1
"
 in 
    let (i, islands) = (tokenizer string) in 
    (*print_string (string_of_island_list islands);*)
    let print =
    match solve islands with
    SolvedSat (bridges) -> "sat " ^ string_of_bridge_list bridges
    | NotSolved -> "um what?"
    | NotSAT -> "unsat" in 
    print_string print

  let () = solve_test ()

let conn_test_2 () =
  let string = 
    "9 
    4.4..2..3
    .........
    6.8.4..1.
    ......1.3
    ..2.2..1.
    4..3.2...
    ......2.3
    .1.5.4...
    3.3.2.3.2
    " in 
      let (i, islands) = (tokenizer string) in 
      let possible_bridges = make_possible_bridge_list islands in 
      let connected_sat_statments_check = connected_sat_statments islands possible_bridges in
    
      print_string (turn_sat_readable connected_sat_statments_check possible_bridges)




let interactive_solve dim islands =
  Printf.printf "Solving puzzle:\n";
  Printf.printf "%s\n" (print_solution dim islands []);
  match solve islands with
    SolvedSat x -> Printf.printf "Solved! Puzzle SAT. Here's the solution:\n%s%s\n" (print_solution dim islands x) (print_bridge_list x)
  | NotSAT -> Printf.printf "Puzzle unSAT.\n"
  | NotSolved -> Printf.printf "Solver output indeterminate. Sorry.\n"



let remove_first_char (s: string) : string =
  if String.length s > 1 then
    String.sub s 1 (String.length s - 1)
  else
    ""

let solve_from_file file =
  let f = open_in file in
  let s = really_input_string f (in_channel_length f) in
  close_in f;
  let (n, isl) = tokenizer (remove_first_char s) in
  interactive_solve n isl

let rec create_puzzle dim islands =
  Printf.printf "Puzzle:\n%s\nWhat is the value of the island to be added?\nType 0 if you are done and want to solve the puzzle.\n" (print_solution dim islands []);
  match read_int () with
    0 -> interactive_solve dim islands
  | v -> if v < 0 || v > 9 then (Printf.printf "ERR: v must be 0 < v < 10!\n"; create_puzzle dim islands)
         else
           (Printf.printf "Now first type the X and then the Y coordinate (separated by a new line). The top-left corner is (0,0).\n";
            let x = read_int () in
            let y = read_int () in
            if x < 0 || y < 0 || x >= dim || y >= dim then
              (Printf.printf "ERR: Make sure both values are between 0 and %i." (dim-1);
              create_puzzle dim islands)
            else
              create_puzzle dim ((Island (v, Pos (x, y)))::islands))

let editor _ =
  Printf.printf "Entering the puzzle creator, since you didn't give an input file.\n";
  Printf.printf "Welcome to the puzzle creator. What is the dimension of the puzzle?\n";
  create_puzzle (read_int ()) []

let main =
  match Array.length Sys.argv with
    1 -> editor ()
  | 2 -> solve_from_file (Sys.argv.(1))
  | _ -> Printf.printf "Usage: ocaml project1.ml [INPUT]\n\nInvocation without input file opens the interactive puzzle creator.\n"

