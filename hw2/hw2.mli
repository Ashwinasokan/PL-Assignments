(* Your hw2.ml file must provide these. *) 
type termination =
  | Normal of Imp.state
  | Exceptional of Imp.state * int
val eval_com : Imp.com -> Imp.state -> termination 
val step_com : Imp.com -> Imp.state -> Imp.com * Imp.state
