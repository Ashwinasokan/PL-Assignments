(* CSCI 5535
 * 
 * YOUR NAME HERE (write your name here so that if the files get mixed up
 * somehow I can still give you credit for your work)
 *  
 * Edit this file according to the instructions in the homework and then
 * submit a renamed copy of it.
 *)

(*
 * Put the code for your interpreter in this file. Your interpreter should
 * be based on the denotational semantics for Regular Expressions that 
 * you developed in the homework. 
 *)

open Re (* re.ml has the definitions for our regexp datatype *) 

(* 
 * "matches re str" returns a full set of suffixes 'y' such that
 * 'x''y' = str and re matches 'x'. Thus if re does not match any prefix 
 * of 'str' it should return the emptyset. 
 *)
let rec matches (re : re) (s : re_string) : stringset = match re with
  | Empty -> singleton s 
  | Char(c) ->
      begin match s with
      | c' :: rest when c = c' -> singleton rest 
      | _ -> emptyset 
      end 
  | AnyChar -> 
      begin match s with
      | c' :: rest -> matches(Char(c')) s
      | _ -> emptyset
      end
  | Question(re) -> matches (Or(re,Empty)) s
  | CharRange(ch1,ch2) -> 
      begin match s with
   	| c' :: rest ->
     	   let rec range ch1 ch2 = 
	   	let asci1 = Char.code ch1 in 
		let asci2= Char.code ch2 in
      		if (asci1 > asci2) then
			emptyset
      		else
        	let match1 = (matches (Char(ch1)) s) in
			if is_empty match1 then
				range (Char.chr (asci1+1)) ch2
	  		else
				match1;
      	   in range ch1 ch2 
	| _ -> emptyset
      end
  | Concat(r1, r2) -> begin
    let match1 = matches r1 s in
    if is_empty match1 then
	emptyset
    else
	Re.fold (fun elt a -> union a (matches r2 elt)) match1 emptyset
    end
  | Or(r1,r2) -> union (matches r1 s) (matches r2 s)
  | Plus(re)->
    matches (Concat(re,Star(re))) s
  | Star(re) ->
    union (singleton s)(matches (Concat(re,Star(re))) s)
  | _ -> Printf.printf "Error: RE %s unimplemented!\n"
    (re_to_str re) ; exit 1


