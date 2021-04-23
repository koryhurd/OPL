type typ =
    | TBool
    | TInt
    | TArrow of typ * typ

  type exp =
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp
    | Var of string
    | Lambda of string * typ * exp
    | Apply of exp * exp

  type type_environment = (string * typ) list

  exception Eval_error
  exception Type_error
  exception Substitution_error
  exception Print_error

  let rec free_variables (e : exp) =
    match e with
    | Var x -> [x]
    | Num n -> []
    | True -> []
    | False -> []
    | Plus(e1, e2) -> List.sort_uniq compare((free_variables e1) @ (free_variables e2))
    | Mult(e1, e2) -> List.sort_uniq compare((free_variables e1) @ (free_variables e2))
    | Apply(e1,e2) -> List.sort_uniq compare((free_variables e1) @ (free_variables e2))
    | IsZero(e) -> free_variables e
    | If(e, e1, e2) -> List.sort_uniq compare((free_variables e) @ (free_variables e1) @ (free_variables e2))
    | Lambda(var, typ, body) -> List.filter(fun y -> var <> y) ([var]);;


  let rec substitution (e1 : exp) (x : string) (e2 : exp) =
    match e1 with
    | Num i -> Num i
    | True -> True
    | False -> False
    | Plus(exp1,exp2) -> Plus(substitution exp1 x e2, substitution exp2 x e2)
    | Mult(exp1,exp2) -> Mult(substitution exp1 x e2, substitution exp2 x e2)
    | Apply(exp1,exp2) -> Apply(substitution exp1 x e2, substitution exp2 x e2)
    | IsZero(exp1) -> IsZero(substitution exp1 x e2)
    | If(exp1, exp2, exp3) -> If(substitution exp1 x e2, substitution exp2 x e2, substitution exp3 x e2)
    | Var y -> if y = x then e2 else Var y
    | Lambda(var, typ, body) ->
                      if ( (var=x) || (List.mem var (free_variables e2)) ) then Lambda(var,typ,body)
                      else if (not (List.mem var (free_variables e1))) && var!=x then Lambda(var,typ, (substitution body x e2))
                      else raise Substitution_error;;

  let rec step (e : exp) =
    match e with
    | Num i -> raise Eval_error
    | Var s -> raise Eval_error
    | True -> raise Eval_error
    | False -> raise Eval_error
    | Lambda(var, typ, body) -> raise Eval_error
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
    | Apply(Lambda(var, typ, body), True) -> substitution body var True
    | Apply(Lambda(var, typ, body), False) -> substitution body var False
    | Apply(Lambda(var, typ, body), Num i) -> substitution body var (Num i)
    | Apply(Lambda(var, typ, body), Lambda(v, t, b)) -> substitution body var (Lambda(v, t, b))
    | Apply(Lambda(var, typ, body), e2) -> Apply(Lambda(var, typ, body), step e2)
    | Apply(e1, e2) -> Apply(step e1, e2)
    | IsZero(False) -> raise Eval_error
    | IsZero(e1) -> let e1' = step e1 in IsZero(e1')
    | If(True, e1, e2) -> e1
    | If(False, e1, e2) -> e2
    | If(e, e1, e2) -> (match e with
      | Num i -> raise Eval_error
      | _ -> let e' = step e in (If(e', e1, e2)));;

  let rec multi_step (e : exp) =
    match e with
    | Num i -> Num i
    | True -> True
    | False -> False
    | Lambda(var, typ, body) -> Lambda(var, typ, body)
    | _ -> let exprafteronestep = step e in
            multi_step exprafteronestep;;


  let rec type_check (te : type_environment) (e : exp) =
    match e with
    | True -> TBool
    | False -> TBool
    | Num i -> TInt
    | Var x -> (match te with
      |  [x, TInt] -> TInt
      |  [x, TBool] -> TBool
      |  [x, TArrow(y,z)] -> TArrow(y,z)
      |  _ -> raise Type_error
      )
    | IsZero(e1) ->
      if type_check te e1 = TBool then raise Type_error
      else if type_check te e1 = TInt then TBool
      else let e1' = step e1 in type_check te e1'
    | Plus(e1, e2) ->
      if type_check te e1 = TBool || type_check te e2 = TBool  then raise Type_error
      else if type_check te e1 = TInt && type_check te e2 = TInt then TInt
      else if type_check te e1 = TInt then let e2' = step e2 in type_check te e2'
      else if type_check te e2 = TInt then let e1' = step e1 in type_check te e1'
      else let e1' = step e1 in type_check te e1'
    | Mult(e1, e2) ->
      if type_check te e1 = TBool || type_check te e2 = TBool  then raise Type_error
      else if type_check te e1 = TInt && type_check te e2 = TInt then TInt
      else if type_check te e1 = TInt then let e2' = step e2 in type_check te e2'
      else if type_check te e2 = TInt then let e1' = step e1 in type_check te e1'
      else let e1' = step e1 in type_check te e1'
    | If(e, e1, e2) ->
      if type_check te e = TInt || type_check te e1 = TBool || type_check te e2 = TBool  then raise Type_error
      else if type_check te e = TBool && type_check te e1 = TInt && type_check te e2 = TInt then TInt
      else if type_check te e = TBool && type_check te e1 = TInt then let e2' = step e2 in type_check te e2'
      else if type_check te e = TBool && type_check te e2 = TInt then let e1' = step e1 in type_check te e1'
      else let e' = step e in type_check te e'
    | Apply(Lambda(var1, typ1, body1), Lambda(var2, typ2, body2)) ->
      let e' = step (Apply(Lambda(var1, typ1, body1), Lambda(var2, typ2, body2))) in type_check te e'
    | Apply(e1, Lambda(var, typ, body)) ->
      raise Type_error
    | Apply(e1, e2) ->
      let e' = step (Apply(e1, e2)) in type_check te e'
    | Lambda(var, typ, body) ->
      TArrow(typ, (type_check ((var, typ)::te) body));;


  let rec string_of_type (t : typ) =
    match t with
    | TBool -> "TBool"
    | TInt -> "TInt"
    | TArrow(t1, t2) -> "TArrow(" ^ string_of_type t1 ^ ", " ^ string_of_type t2 ^ ")";;

  let rec string_of_fun (e : exp) =
    match e with
    | IsZero (expression) ->
       "IsZero(" ^ string_of_fun expression ^ ")"
    | Plus (left, right) ->
       "Plus(" ^ string_of_fun left ^ ", " ^ string_of_fun right ^ ")"
    | Mult (left, right) ->
       "Mult(" ^ string_of_fun left ^ ", " ^ string_of_fun right ^ ")"
    | If (left, center, right) ->
       "If(" ^ string_of_fun left ^ ", " ^ string_of_fun center ^ ", " ^ string_of_fun right ^ ")"
    | Lambda(var, typ, body) -> "Lambda( \"" ^ var ^ "\", " ^ string_of_type typ ^ ", " ^ string_of_fun body ^ ")"
    | Apply(left, right) -> "Apply (" ^ string_of_fun left ^ ", " ^ string_of_fun right ^ ")"
    | True -> "True"
    | False -> "False"
    | Var s -> "Var \"" ^ s ^"\""
    | Num i -> "Num " ^ string_of_int i;;

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
    | Lambda(var, typ, body) -> "Lambda( \"" ^ var ^ "\", " ^ string_of_type typ ^ ", " ^ string_of_fun body ^ ")"
    | Apply(left, right) -> raise Print_error
    | True -> "True"
    | False -> "False"
    | Var s -> "(Var " ^ s ^")"
    | Num i -> "(Num " ^ string_of_int i ^ ")";;

  let rec print_list_string myList = match myList with
    | [] -> print_endline " "
    | head::body ->
    begin
    print_endline head;
    print_list_string body
    end;;

  let print_multi_step e =
      print_endline(string_of_exp(multi_step e));;

  let print_substitution e1 x e2 =
      print_endline(string_of_fun(substitution e1 x e2));;

  let print_type_check e =
      print_endline(string_of_type(type_check[] e));;
