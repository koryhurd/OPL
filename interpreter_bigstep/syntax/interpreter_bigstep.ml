type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp


  let rec string_of_exp (e : exp) =
    match e with
    | IsZero (expression) ->
       "(isZero " ^ string_of_exp expression ^ ")"
    | Plus (left, right) ->
       "(" ^ string_of_exp left ^ " + " ^ string_of_exp right ^ ")"
    | Mult (left, right) ->
       "(" ^ string_of_exp left ^ " * " ^ string_of_exp right ^ ")"
    | If (left, center, right) ->
       "if " ^ string_of_exp left ^ " then " ^ string_of_exp center ^ " else " ^ string_of_exp right
    | True ->
       "true"
    | False ->
       "false"
    | Num i -> string_of_int i;;

  let print_expr e =
    print_endline (string_of_exp e);;

  print_expr (Num 3);;
  print_expr (True);;
  print_expr (False);;
  print_expr (Plus (Num 3, Num 2));;
  print_expr (Mult (Num 3, Num 2));;
  print_expr (Plus (Num 3, Plus (Num 3, Mult (Num 2, Plus (Num 3, Num 2)))));;
  print_expr (If (True, Num 3, Num 5));;
  print_expr (If (False, Plus (Num 3, Num 2), Plus (Num 5, Num 1)));;
  print_expr (If (Plus (False, True), Plus (Num 3, False), Mult (Num 3, Num 1)));;
  print_expr (If (IsZero (Num 1), Plus (Num 3, Num 2), Plus (Num 5, Num 1)));;
  print_expr (IsZero(Mult (Num 3, Num 5)));;
  print_expr (IsZero( If( IsZero(Num 1), Plus (Num 3, Num 5), Plus (Num 5, Num 1))));;
  print_expr (Plus (Num 3, If (IsZero (Num 1), Plus (Num 3, Num 5), Plus (Num 5, Num 1))));;
  print_expr (Plus (Num 3, Mult (If (IsZero(Num 1), Plus (Num 3, Num 5), Plus (Num 5, Num 1)), IsZero(True))));;
  print_expr (If (If (True, True, False), Plus (Num 3, Num 2), Plus (Num 5, Num 1)));;
  print_expr (If (True, If (IsZero( Mult (Num 3, Num 5)), Plus (Num 3, Num 2), Plus (Num 5, Num 1)), If (True, Mult (Num 3, Num 2), Mult (Num 2, Plus (Num 3, Num 2)))));;
