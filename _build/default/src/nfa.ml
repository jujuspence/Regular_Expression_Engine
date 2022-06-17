open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
union (List.fold_left (fun a x -> 
	List.fold_left (fun b trans -> match trans with
	|(i,None,k) -> if x==i && s = None then k::b else b
	|(i,Some j,k) -> if x==i && s= (Some j) then k::b else b
) a nfa.delta)[] qs) []


let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = 
List.fold_right (fun x a-> union (move nfa x None) a 
) [qs] qs


let accept (nfa: ('q,char) nfa_t) (s: string) : bool = let lst = explode s in 
let r = List.fold_left (fun a x-> let str_op = Some x in
let b = move nfa a str_op in 
if a<>[] && b<>[] then e_closure nfa b else a
) [nfa.q0] lst in
if (intersection r nfa.fs)<>[] then true else false



(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  List.fold_left (fun a s -> (e_closure nfa (move nfa qs (Some s)))::a) [] nfa.sigma 


let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
List.fold_left (fun a x-> match x with
| (i,j,k) -> if (elem i qs)&&(j<>None) then (qs,j,[k])::a else a
) [] nfa.delta


let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
let a = intersection qs nfa.fs in
if a==[] then [] else [qs]
 

let rec nfa_to_dfa_helper nfa dfa work all_work= 
 match work with
| [] -> dfa
| x::xs -> let dfa2 = {
sigma = dfa.sigma;
qs = remove [] (union dfa.qs (new_states nfa x));
q0 = dfa.q0;
fs = dfa.fs @ (new_finals nfa x);
delta = dfa.delta @ (new_trans nfa x)
} in
if (subset (new_states nfa (e_closure nfa x)) all_work) 
then nfa_to_dfa_helper nfa dfa2 xs all_work
else nfa_to_dfa_helper nfa dfa2 (union (new_states nfa (e_closure nfa x)) xs) (union (new_states nfa (e_closure nfa x)) all_work)


let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
nfa_to_dfa_helper nfa dfa work work

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = 
let dfa = {
sigma = nfa.sigma;
qs = [e_closure nfa [nfa.q0]];
q0 = [nfa.q0];
fs = [];
delta = [];
} in
nfa_to_dfa_step nfa dfa dfa.qs;


