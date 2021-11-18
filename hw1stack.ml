type 'a stack = 'a list;;

let start (f: 'a stack -> 'b) = f [];;

let push x (stack : 'a stack) (f: 'a stack -> 'b) =
  f (x :: stack);;

let pop (stack : 'a stack) (f: 'a stack -> 'b) =
  match stack with
    | [] -> f []
    | x::stack -> f stack;;

let add (stack : 'a stack) (f: 'a stack -> 'b) =
  match stack with
    | [] -> f []
    | [x] -> f [x]
    | x1::x2::stack -> f ((x1 + x2)::stack);;

let mult (stack : 'a stack) (f: 'a stack -> 'b) =
  match stack with
    | [] -> f []
    | [x] -> f [x]
    | x1::x2::stack -> f ((x1 * x2)::stack);;

let clone (stack : 'a stack) (f: 'a stack -> 'b) =
  match stack with
    | [] -> f []
    | x::stack -> f (x::x::stack);;

let kpop (stack : 'a stack) (f: 'a stack -> 'b) =
  match stack with
    | [] -> f []
    | k::stack -> let rec kpop_aux k stack =
                    if k <= 0 then stack
                    else match stack with
                      | [] -> []
                      | x::stack -> kpop_aux (k - 1) stack in
                  f (kpop_aux k stack);;

let halt (stack : 'a stack) = stack;;

