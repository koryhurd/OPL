type exp =
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp

  exception Eval_error

  let rec step (e : exp) =
    match e with
    | Num i -> raise Eval_error
    | True -> raise Eval_error
    | False -> raise Eval_error
    | Plus(Num i, Num j) ->
      let n1 = i + j in Num n1
    | Plus(e1, Num i) ->
      if e1 = True || e1 = False then raise Eval_error else
      let e1' = step e1 in Plus(e1', Num i)
    | Plus(Num i, e1) ->
      if e1 = True || e1 = False then raise Eval_error else
      let e1' = step e1 in Plus(Num i, e1')
    | Plus(e1, e2) ->
      if e1 = True || e1 = False || e2 = True || e2 = False then raise Eval_error else
      let e1' = step e1 in Plus(e1', e2)
    | Mult(Num i, Num j) ->
      let n1 = i * j in Num n1
    | Mult(e1, Num i) ->
      if e1 = True || e1 = False then raise Eval_error else
      let e1' = step e1 in Mult(e1', Num i)
    | Mult(Num i, e1) ->
      if e1 = True || e1 = False then raise Eval_error else
      let e1' = step e1 in Mult(Num i, e1')
    | Mult(e1, e2) ->
      if e1 = True || e1 = False || e2 = True || e2 = False then raise Eval_error else
      let e1' = step e1 in Mult(e1', e2)
    | IsZero(Num i) ->
      if i = 0 then True
      else if i != 0 then False
      else raise Eval_error
    | IsZero(True) -> raise Eval_error
    | IsZero(False) -> raise Eval_error
    | IsZero(e1) -> let e1' = step e1 in IsZero(e1')
    | If(True, e1, e2) -> e1
    | If(False, e1, e2) -> e2
    | If(e, e1, e2) -> match e with
      | Num i -> raise Eval_error
      | _ -> let e' = step e in (If(e', e1, e2));;

  let rec multi_step (e : exp) =
    match e with
    | Num i -> Num i
    | True -> True
    | False -> False
    | _ -> let exprafteronestep = step e in
            multi_step exprafteronestep;;

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
    | True -> "True"
    | False -> "False"
    | Num i -> "(Num " ^ string_of_int i ^ ")";;

  let print_multi_step e =
      print_endline(string_of_exp(multi_step e));;
