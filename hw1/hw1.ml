(*: CSCI 5535
 * 
 * ASHWIN ASOKAN (write your name here so that if the files get mixed up
 * somehow I can still give you credit for your work)
 *  
 * Edit this file according to the instructions in the homework and then
 * submit a renamed copy of it. Name the copy "hw1-your_identikey.ml". For
 * example, if you are Grace Hopper, send in something like "hw1-hopperg.ml". 
*)

(*
 * Put the code for your interpreter in this file. Your interpreter should
 * be based on the big-step (natural-style) operational semantics for IMP
 * that we went over in class (and in Winskel's book). 
 *
 * This skeleton file includes one implementation of states (based on
 * OCaml's Map) and evaluations for AExps. 
 *)

open Imp (* imp.ml has the definitions for our IMP datatypes *) 

(* Our operational semantics has a notion of 'state' (sigma). The type
 * 'state' is a map (LocMap.t) from 'loc' to 'int'.
 * 
 * See http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.S.html
 * 
 * The helper functions below wrap the library below so that you need
 * not use the library functions directly.
*)
module LocMap = Map.Make(struct
                  type t = loc
                  let compare = compare
                end)
type state = int LocMap.t

(* The empty state. *)
let empty_state: state = LocMap.empty

(* Given a state sigma, return the current value associated with
   variable 'x'. For our purposes all uninitialized variables start at 0. *)
let lookup (sigma: state) (x: loc) : int = 
  try
    LocMap.find x sigma
  with Not_found -> 0 

(* Given a state sigma, return a new state like sigma except that variable x
   maps to integer n. *)
let update (sigma: state) (x: loc) (n: int) : state = LocMap.add x n sigma

(* Evaluates an aexp given the state 'sigma'. *) 
let rec eval_aexp (a: aexp) (sigma: state) : int = match a with
  | Const n -> n
  | Var(loc) -> lookup sigma loc
  | Add(a0,a1) -> eval_aexp a0 sigma + eval_aexp a1 sigma
  | Sub(a0,a1) -> eval_aexp a0 sigma - eval_aexp a1 sigma
  | Mul(a0,a1) -> eval_aexp a0 sigma * eval_aexp a1 sigma
  | Div(a0,a1) -> eval_aexp a0 sigma / eval_aexp a1 sigma
  | Mod(a0,a1) -> eval_aexp a0 sigma mod eval_aexp a1 sigma

(* Evaluates a bexp given the state 'sigma'. *) 
let rec eval_bexp (b: bexp) (sigma: state) : bool = match b with
  | True -> true
  | False -> false 
  | EQ(aexp1, aexp2) -> eval_aexp aexp1 sigma = eval_aexp aexp2 sigma
  | LE(aexp1, aexp2) -> eval_aexp aexp1 sigma <= eval_aexp aexp2 sigma
  | Not bexp -> not (eval_bexp bexp sigma)
  | And(bexp1, bexp2) -> eval_bexp bexp1 sigma && eval_bexp bexp2 sigma
  | Or(bexp1, bexp2) -> eval_bexp bexp1 sigma || eval_bexp bexp2 sigma

(* Evaluates a com given the state 'sigma'. *) 
let rec eval_com (c: com) (sigma:state) : state = match c with
  | Skip -> sigma
  | Set(x, aexp) ->
      let value = eval_aexp aexp sigma in
        update sigma x value;
  | Seq(com1, com2)  ->
      let value = eval_com com1 sigma in
        eval_com com2 value
  | If(bexp, com1, com2) ->
      eval_com (if eval_bexp bexp sigma then com1 else com2) sigma
  | While(bexp, com) -> 
      if eval_bexp bexp sigma
      then (let sigma' = eval_com com sigma in 
              eval_com (While(bexp,com)) sigma')
      else sigma
  | Let(x,aexp,com) ->
      let prev = lookup sigma x in
        let value = eval_aexp aexp sigma in
                let sigma' = update sigma x value in 
                  update (eval_com com sigma') x prev;
  | Print (aexp) ->
      let value = eval_aexp aexp sigma in
        Printf.printf "%d" value;
        sigma
