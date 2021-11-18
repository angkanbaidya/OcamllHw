let rec pow x n =
  if n = 0 then 1
  else x * pow x (n - 1);;




let rec floatpow x n =
  if n = 0 then 1.0
  else x *. floatpow x (n - 1);;

let compress = function
  | [] -> []
  | x::xs -> let rec compress_aux last = function
               | [] -> []
               | x::xs -> if x = last then
                            compress_aux last xs
                          else
                            x :: compress_aux x xs in
             x :: compress_aux x xs;;

let rec remove_if xs f =
  match xs with
    | [] -> []
    | x::xs -> if f x then
                 remove_if xs f
               else
                 x :: remove_if xs f;;

let rec slice l s e =
  if e = 0 then []
  else if s = 0 then
    match l with
      | [] -> []
      | h :: r -> h :: (slice r 0 (e - 1))
  else
    match l with
      | [] -> []
      | h :: r -> slice r(s - 1)(e - 1);;

let equivs p l =
  let rec equivs_aux p l c =
    match l with
      | [] -> c
      | x::xs ->
        let rec put_in_class p i = function
          | (y::ys)::yss -> if p i y then [(y::ys) @ [i]] @ yss
                     else (y::ys) :: put_in_class p i yss
          | _ -> [[i]] in
        equivs_aux p xs (put_in_class p x c) in
  equivs_aux p l [];;

let isprime n =
  if n < 2 then false
  else
    let rec isprime_aux n x =
      if float_of_int(x) > sqrt(float_of_int(n)) then
        true
      else
        if n mod x = 0 then
          false
        else
          isprime_aux n (x + 1) in
    isprime_aux n 2;;

let prime_list n =
  let rec prime_list_aux n count =
    if count > n then
      []
    else
      if isprime count then
        count :: prime_list_aux n (count + 1)
      else
        prime_list_aux n (count + 1) in
  prime_list_aux n 2;;

let goldbachpair n =
  if n mod 2 = 1 then
    (0, 0)
  else
    let rec goldbachpair_aux n xs ys =
      match xs, ys with
        | [], _ -> (0, 0)
        | x::xs, [] -> goldbachpair_aux n xs (prime_list n)
        | x::xs, y::ys -> if x + y = n then (x, y)
                        else goldbachpair_aux n (x::xs) ys in
    goldbachpair_aux n (prime_list n) (prime_list n);;

let rec equiv_on f g = function
  | [] -> true
  | x::xs -> if f x = g x then
               equiv_on f g xs
             else
               false;;

let rec pairwisefilter cmp lst =
  match lst with
    | [] -> []
    | [l] -> [l]
    | l1::l2::lst -> cmp l1 l2 :: (pairwisefilter cmp lst);;

let rec polynomial ys x =
  match ys with
    | [] -> 0
    | (y1,y2)::ys -> y1 * (pow x y2) + polynomial ys x;;

let rec map f = function
  | [] -> []
  | x::xs -> f x :: map f xs;;

let rec powerset = function
  | [] -> [[]]
  | [x] -> [[x]; []]
  | x::xs -> map (fun xs -> x::xs) (powerset xs) @ powerset xs;;


  
