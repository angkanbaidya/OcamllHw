type bool_expr =
  | Lit of string
  | Not of bool_expr

  
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let truth_table a b expr =
  let rec eval a b expr a_bool b_bool =
    match expr with
      | Lit c -> if a = c then a_bool
                 else if b = c then b_bool
                 else false
      | Not e -> not (eval a b e a_bool b_bool)
      | And (e1, e2) -> (eval a b e1 a_bool b_bool) && (eval a b e2 a_bool b_bool)
      | Or (e1, e2) ->  (eval a b e1 a_bool b_bool) || (eval a b e2 a_bool b_bool) in

  let eval_expr = eval a b expr in
  [(true, true, eval_expr true true); (true, false, eval_expr true false);
  (false, true, eval_expr false true); (false, false, eval_expr false false)];;




  