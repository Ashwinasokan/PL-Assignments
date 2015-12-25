(* CSCI 5535
 * 
 * YOUR NAME HERE (write your name here so that if the files get mixed up
 * somehow I can still give you credit for your work)
 *  
 * Edit this file according to the instructions in the homework and then
 * submit a renamed copy of it. Name the copy "your_last_name-hw2.ml". For
 * example, if you are Grace Hopper, send in "hopper-hw2.ml". 
 *)

(*
 * Put the code for your interpreter in this file. 
 *
 * This skeleton file includes one implementation of states (based on
 * OCaml's Hashtbl) and evaluations for AExps. 
 *)

open Imp (* imp.ml has the definitions for our IMP datatypes *) 


(***** Big-Step Interpreter *****)

(* 
 * A command may either terminate normally (as before) OR it may terminate
 * exceptionally (and the exception has an integer value).
 *)
type termination = 
  | Normal      of state
  | Exceptional of state * int

(* Evaluates an aexp given the state 'sigma'. *) 
let rec eval_aexp (a: aexp) (sigma: state) : int = match a with
  | Const n -> n
  | Var loc -> lookup sigma loc 
  | Add (a0,a1) -> 
    let n0 = eval_aexp a0 sigma in
    let n1 = eval_aexp a1 sigma in
    n0 + n1
  | Sub (a0,a1) -> 
    let n0 = eval_aexp a0 sigma in
    let n1 = eval_aexp a1 sigma in
    n0 - n1
  | Mul (a0,a1) -> 
    let n0 = eval_aexp a0 sigma in
    let n1 = eval_aexp a1 sigma in
    n0 * n1
  | Div (a0,a1) -> 
    let n0 = eval_aexp a0 sigma in
    let n1 = eval_aexp a1 sigma in
    n0 / n1

(* Evaluates a bexp given the state 'sigma'. *) 
let rec eval_bexp (b: bexp) (sigma: state) : bool = match b with
  | True -> true
  | False -> false 
  | EQ (a0,a1) ->
      let n0 = eval_aexp a0 sigma in
      let n1 = eval_aexp a1 sigma in
      n0 = n1
  | LE (a0,a1) ->
      let n0 = eval_aexp a0 sigma in
      let n1 = eval_aexp a1 sigma in
      n0 <= n1
  | Not b ->
      not (eval_bexp b sigma)
  | And (b0,b1) ->
      eval_bexp b0 sigma && eval_bexp b1 sigma
  | Or (b0,b1) ->
      eval_bexp b0 sigma || eval_bexp b1 sigma

(* Evaluates a com given the state 'sigma'. *) 
let rec eval_com (c: com) (sigma: state) : termination = match c with
  | Skip -> Normal sigma
  | Print(a) ->
      let n = eval_aexp a sigma in
      Printf.printf "%d " n;
      Normal sigma
  | Set(x, aexp) ->
      let value = eval_aexp aexp sigma in
      let sigma' = update sigma x value in
        Normal sigma'
  | Seq(com1, com2)  ->
      let value = eval_com com1 sigma in
        begin match value with
          | Normal(sigma') -> eval_com com2 sigma'
          | Exceptional(sigma',n) -> Exceptional(sigma', n)
        end
  | If(bexp, com1, com2) ->
      eval_com (if eval_bexp bexp sigma then com1 else com2) sigma
  | While(bexp, com) -> 
      if eval_bexp bexp sigma
      then (let value = eval_com com sigma in 
              begin match value with
                | Normal(sigma') -> eval_com (While(bexp,com)) sigma'
                | Exceptional(sigma',n) -> Exceptional(sigma', n)
              end)
      else Normal sigma
  | Throw(aexp) ->
      let value = eval_aexp aexp sigma in
        Exceptional(sigma, value);
  | TryCatch(com1,loc,com2) ->
      let value = eval_com com1 sigma in
        begin match value with
          | Normal(sigma) -> value;
          | Exceptional(sigma,n) ->
              eval_com com2 (update sigma loc n);
        end
  | AfterFinally(com1, com2) ->
      let com1_value = eval_com com1 sigma in
        begin match com1_value with
          | Normal(sigma') -> 
              eval_com com2 sigma';
          | Exceptional(sigma',n1) ->
              let com2_value = eval_com com2 sigma' in
                match com2_value with
                  | Normal(sigma'') ->
                      Exceptional(sigma',n1);
                  | Exceptional(sigma'',n2) ->
                      Exceptional(sigma'',n2);
        end
  | Let(x,aexp,com) ->
      let prev = lookup sigma x in
        let value = eval_aexp aexp sigma in
                let sigma' = update sigma x value in
                        let value = eval_com com sigma' in 
                                begin match value with
                                  | Normal(sigma'') ->
                                      Normal(update sigma'' x prev)
                                  | Exceptional(sigma'',n) -> 
                                      Exceptional(update sigma'' x prev,n);
                                end

(***** Small-Step Interpreter *****)

(* This exception indicates when a step does not exist because the input
   is a value. *)
exception Value

(* Takes a step of evaluation of an aexp given the state 'sigma'. *) 
let rec step_aexp (a: aexp) (sigma: state) : aexp = match a with
  | Const _ -> raise Value
  | Var x -> Const (lookup sigma x)
  | Add (Const n0, Const n1) -> Const (n0 + n1)
  | Add (Const n0, a1) ->
    let a1' = step_aexp a1 sigma in Add (Const n0, a1')
  | Add (a0, a1) ->
    let a0' = step_aexp a0 sigma in Add (a0', a1)
  | Sub (Const n0, Const n1) -> Const (n0 - n1)
  | Sub (Const n0, a1) ->
    let a1' = step_aexp a1 sigma in Sub (Const n0, a1')
  | Sub (a0, a1) ->
    let a0' = step_aexp a0 sigma in Sub (a0', a1)
  | Mul (Const n0, Const n1) -> Const (n0 * n1)
  | Mul (Const n0, a1) ->
    let a1' = step_aexp a1 sigma in Mul (Const n0, a1')
  | Mul (a0, a1) ->
    let a0' = step_aexp a0 sigma in Mul (a0', a1)
  | Div (Const n0, Const n1) -> Const (n0 / n1)
  | Div (Const n0, a1) ->
    let a1' = step_aexp a1 sigma in Div (Const n0, a1')
  | Div (a0, a1) ->
    let a0' = step_aexp a0 sigma in Div (a0', a1)

(* Takes a step of evaluation of a bexp given the state 'sigma'. *) 
let rec step_bexp (b: bexp) (sigma: state) : bexp = match b with
  | True
  | False -> raise Value
  | EQ (Const n0, Const n1) -> if n0 = n1 then True else False
  | EQ (Const n0, a1) ->
    let a1' = step_aexp a1 sigma in EQ (Const n0, a1')
  | EQ (a0, a1) ->
    let a0' = step_aexp a0 sigma in EQ (a0', a1)
  | LE (Const n0, Const n1) -> if n0 <= n1 then True else False
  | LE (Const n0, a1) ->
    let a1' = step_aexp a1 sigma in LE (Const n0, a1')
  | LE (a0, a1) ->
    let a0' = step_aexp a0 sigma in LE (a0', a1)
  | Not True -> False
  | Not False -> True
  | Not b0 ->
    let b0' = step_bexp b0 sigma in Not b0'
  | And (False, _)
  | And (True, False) -> False
  | And (True, True) -> True
  | And (True as b0, b1) ->
    let b1' = step_bexp b1 sigma in And (b0, b1')
  | And (b0, b1) ->
    let b0' = step_bexp b0 sigma in And (b0', b1)
  | Or (True, _) 
  | Or (False, True) -> True
  | Or (False, False) -> False
  | Or (False as b0, b1) ->
    let b1' = step_bexp b1 sigma in Or (b0, b1')
  | Or (b0, b1) ->
    let b0' = step_bexp b0 sigma in Or (b0', b1)

(* Takes a step of evaluation a com given the state 'sigma'. *) 
let rec step_com (c: com) (sigma: state) : com * state = match c with
  | Skip
  | Throw (Const _) -> raise Value
  | Throw (aexp) ->
      let value = step_aexp aexp sigma in
        (Throw (value),sigma)
  | Print (Const n) -> Printf.printf "%d " n; (Skip, sigma)
  | Print (aexp) -> 
      let value = step_aexp aexp sigma in (Print (value),sigma)
  | Set (x, Const n) -> (Skip, update sigma x n)
  | Set (x, aexp) ->
      let value = step_aexp aexp sigma in (Set (x,value),sigma)
  | Seq(Throw e, com2)  -> (Throw (e),sigma)
  | Seq(Skip, com2)  -> (com2,sigma)
  | Seq(com1, com2)  ->
      let (com1',sigma') = step_com com1 sigma in
        (Seq(com1', com2),sigma')
  | If(True, com1, com2) -> (com1, sigma)
  | If(False, com1, com2) -> (com2, sigma)
  | If(bexp, com1, com2) ->
      let value = step_bexp bexp sigma in (If (value,com1,com2),sigma)
  | While(bexp, com) ->
      If(bexp,Seq(com,While(bexp,com)),Skip),sigma
  | Let(x,Const n,Skip) -> (Skip,sigma)
  | Let(x,Const n,Throw Const a) -> (Throw (Const(a)),sigma)
  | Let(x,Const n,Throw aexp) ->
      let prev = lookup sigma x in
      let sigma = update sigma x n in
      let value = step_aexp aexp sigma in
        (Let (x, Const(n),Throw value),update sigma x prev)
  | Let(x,Const n,com) ->
      let prev = lookup sigma x in
      let sigma = update sigma x n in
      let (com,sigma) = step_com com sigma in
      let after = lookup sigma x in
      let sigma = update sigma x prev in
        (Let(x,Const(after),com),sigma)
  | Let(x,aexp,com) ->
      let value = step_aexp aexp sigma in
        (Let(x,value,com),sigma)
  | TryCatch(Skip,loc,com2) -> (Skip,sigma)
  | TryCatch(Throw Const n,loc,Skip) -> (Skip,sigma)
  | TryCatch(Throw Const n,loc,com2) -> 
	let sigma = update sigma loc n in
	    (step_com com2 sigma)
  | TryCatch(Throw aexp,loc,com2) ->
      let value = step_aexp aexp sigma in
        (TryCatch (Throw value, loc, com2),sigma)
  | TryCatch(com1,loc,com2) -> 
      let (com1',sigma) = step_com com1 sigma in 
        (TryCatch (com1', loc, com2),sigma)
  | AfterFinally(Skip,com2) -> (com2,sigma)
  | AfterFinally(Throw e,Skip) -> (Throw(e),sigma)
  | AfterFinally(Throw e,com2) ->
      let (com2',sigma') = step_com com2 sigma in
       (AfterFinally(Throw(e),com2'),sigma')
  | AfterFinally(com1,com2) ->
      let (com1',sigma) = step_com com1 sigma in
        (AfterFinally(com1', com2),sigma)
