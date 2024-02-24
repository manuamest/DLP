open Format
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyReg of (string * ty) list
  | TyList of ty
;;

type 'a context =
  (string * 'a) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmNum of int
  | TmLength of term
  | TmTuple of term list  (*Tuples*)
  | TmPro of term * string
  | TmReg of (string * term) list
  | TmHead of ty * term
  | TmTail of ty * term
  | TmCons of ty * term * term
  | TmNil of ty
  | TmisEmpty of ty * term
  | TmType of ty
;;

type command = 
  Eval of term
  | Bind of string * term
  | BindTy of string * ty
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyArr (ty1, ty2) ->
      "" ^ string_of_ty ty1 ^ "" ^ " -> " ^ "" ^ string_of_ty ty2 ^ ""
  | TyString ->
      "String"
  | TyTuple tyr ->
      let rec print = function
        [] -> ""
        | (ty::[]) -> (string_of_ty ty)
        | (ty::t)  -> (string_of_ty ty) ^ ", " ^ print t
      in "{" ^ (print tyr) ^ "}"

  | TyReg tyr ->
      let rec print = function
        [] -> ""
        |((s,ty)::[]) -> s ^ ":" ^ (string_of_ty ty)
        |((s,ty)::t) -> s ^ ":" ^ (string_of_ty ty) ^ "," ^ print t
      in "{" ^ (print tyr) ^ "}" 

  | TyList ty -> "List[" ^ string_of_ty ty ^ "]" 
;;

exception Type_error of string
;;

let rec subtypeof tm1 tm2 = match (tm1, tm2) with
  | (TyArr(s1, s2), TyArr(t1, t2)) -> ((subtypeof s1 t1) && (subtypeof t2 s2))
  | (TyReg(l1), TyReg(l2)) ->
    let check (x, ty) l =
      try 
        subtypeof ty (List.assoc x l)
    with _ -> false
    in let rec contains l1 l2 = match l1 with
      | [] -> true
      | (h::t) -> (&&) (check h l2) (contains t l2)
      in contains l1 l2
  | (tm1, tm2) -> tm1 = tm2
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try getbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if subtypeof tyT11 tyT2 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2
  
    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if subtypeof tyT11 tyT12 then tyT12
          else raise (Type_error "reult of body not compatible with domain")
      | _ -> raise (Type_error "arrow type expected"))
  
    (* T-String *)
  | TmString _ ->
      TyString

    (* T-Concat *)
  | TmConcat (t1, t2) -> 
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
      else raise (Type_error "argument of concat is not a string")

  | TmNum _ ->
      TyNat
      
    (* T-Length *)
  | TmLength t1 ->
    if typeof ctx t1 = TyString then TyNat
    else raise (Type_error "argument of length is not a string")

    (* T-Tuple *)
  | TmTuple tml ->
      let rec get_types = function
          [] -> []
          | (tm::t) -> ((typeof ctx tm)::(get_types t))
      in TyTuple (get_types tml)

    (* T-Reg *)  
  | TmReg tmr ->
    let rec get_types = function 
      [] -> []
      |((s,tm)::t) -> ((s,typeof ctx tm)::(get_types t))
    in TyReg (get_types tmr) 

    (* T-Proj *)
  | TmPro (t, n) ->
    (match (typeof ctx t, n) with
      | (TyReg (tyr), s) ->
        (try List.assoc s tyr with
        _ -> raise (Type_error ("cannot project "^ s ^ ",this key does not exist in the record"))) 
      | (TyTuple (tyr), s) ->
        (try List.nth tyr (int_of_string s - 1) with
          _ -> raise (Type_error ("cannot project " ^ s ^ ", this key does not exist in the tuple")))
        | _ -> raise (Type_error "Projection is only supported on tuples"))

  | TmCons(ty,h,t) ->
      let tyTh = typeof ctx h in 
        let tyTt = typeof ctx t in 
           if (subtypeof tyTh ty) && (subtypeof tyTt (TyList(ty))) then 
              TyList(ty) else raise (Type_error "elements of list have different types")

  | TmHead (ty,t) ->
      if typeof ctx t = TyList(ty) then ty
      else raise (Type_error ("head is not a List of" ^ (string_of_ty ty)))

  | TmTail (ty,t) ->
      if typeof ctx t = TyList(ty) then TyList(ty)
      else raise (Type_error ("tail is not a List of" ^ (string_of_ty ty)))

  | TmNil ty ->
      TyList ty

  | TmType ty ->
      ty

  | TmisEmpty (ty,t) ->
      if typeof ctx t = TyList(ty) then TyBool
      else raise (Type_error ("Argument of isempty is not a List"))
;;

(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "" ^ string_of_term t1 ^ "" ^
      " then " ^ "" ^ string_of_term t2 ^ "" ^
      "\nelse " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "(pred " ^ "" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "" ^ string_of_term t ^ ""
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "lambda " ^ s ^ " : " ^ string_of_ty tyS ^ ". \n" ^ string_of_term t ^ ""
  | TmApp (t1, t2) ->
      "" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ""
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "fix " ^ "(" ^ string_of_term t ^ ")\n"
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
      "concat " ^ "(" ^ string_of_term t1 ^ ")" ^ " " ^ "(" ^ string_of_term t2 ^ ")"
  | TmNum t ->
    "\"" ^ string_of_int t ^ "\""
  | TmLength t ->
      "length"  ^ "(" ^ string_of_term t ^ ")"
  | TmTuple tml ->
    let rec print = function
      [] -> ""
      |(tm::[]) -> (string_of_term tm)
      |(tm::t) -> (string_of_term tm) ^ ", " ^ print t
    in "{" ^ (print tml) ^ "}"
    
  | TmReg tmr -> 
      let rec print = function 
        [] -> ""
        |((s,tm)::[]) -> s ^ "=" ^ (string_of_term tm)
        |((s,tm)::t) -> s ^ "=" ^ (string_of_term tm) ^ "," ^ print t
      in "{" ^ (print tmr) ^ "}"  

  | TmPro (t, n) ->
      string_of_term t ^ "." ^ n

  | TmCons(ty,h,t) -> 
      "cons[" ^ string_of_ty ty ^ ":" ^ string_of_term h ^ "," ^ (string_of_term t) ^ "]"
  | TmHead (ty,t) ->
      "head[" ^ string_of_ty ty ^ "]" ^ "(" ^ string_of_term t ^ ")"
  
  | TmTail (ty,t) ->
      "tail[" ^ string_of_ty ty ^ "]" ^ "(" ^ string_of_term t ^ ")"
  | TmNil ty ->
      "nil[" ^ string_of_ty ty ^ "]"
  | TmType ty ->
      string_of_ty ty
  | TmisEmpty (ty,t) ->
     "isempty[" ^ string_of_ty ty ^ "]"     
 ;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString _ ->
      []
  | TmConcat (t1, t2) -> 
      lunion (free_vars t1) (free_vars t2)
  | TmNum _->
      []
  | TmLength t ->
    free_vars t
  | TmTuple tml->
    let rec get_free = function
      [] -> []
      |(tm::t) -> lunion (free_vars tm) (get_free t)
    in get_free tml  
  | TmPro (t, n) ->
      free_vars t
  | TmReg tmr ->
    let rec get_free = function
      [] -> []
      |((_,tm)::t) -> lunion (free_vars tm) (get_free t)
    in get_free tmr 

  | TmCons (ty,h,t) ->
      lunion (free_vars h) (free_vars t)

  | TmHead (ty,t) ->
      free_vars t
  
  | TmTail (ty,t) ->
      free_vars t
  | TmNil ty ->
    []
  | TmType ty ->
    []
  | TmisEmpty (ty,t) ->
      free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmString st -> 
      TmString st
  | TmConcat (t1,t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmNum t ->
    TmNum t
  | TmLength t ->
      TmLength (subst x s t)
  | TmTuple tml ->
    let rec subs_rcd = function
      [] -> []
      |(tm::t) -> (subst x s tm)::(subs_rcd t)
    in TmTuple (subs_rcd tml)

  | TmReg tmr ->
      let rec subst_reg = function 
        [] -> []
        |((st,tm)::t) -> (st,(subst x s tm))::(subst_reg t)
      in TmReg (subst_reg tmr)
       
  | TmPro (t,n) ->
      TmPro  (subst x s t, n)

  | TmCons(ty,h,t) ->
      TmCons(ty,(subst x s h),(subst x s t))
  
  | TmHead (ty,t) ->
      TmHead (ty,(subst x s t))

  | TmTail (ty,t) ->
      TmTail (ty,(subst x s t))

  | TmNil ty ->
      tm
  | TmType ty ->
      tm
  | TmisEmpty (ty,t) ->
      TmisEmpty(ty,subst x s t)

;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | t when isnumericval t -> true
  | TmTuple l -> List.for_all(fun t -> isval(t)) l 
  | TmReg [] -> true
  | TmReg l -> List.for_all(fun (s,t) -> isval(t)) l 
  | TmCons(_,h,t) -> (isval h) && (isval t)
  | TmNil _-> true
  | TmType _-> true
  | _ -> false

;;

exception NoRuleApplies
;;

let rec eval1 vctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2)

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 vctx t1 in
      TmFix t1'

    (* E-Concat *)  
  | TmConcat (TmString s1, TmString s2) -> 
      TmString (s1 ^ s2)

  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 vctx t2 in 
        TmConcat (TmString s1, t2')

  | TmConcat (t1, t2) -> 
      let t1' = eval1 vctx t1 in 
        TmConcat (t1', t2) 

    (* E-length*)
  | TmLength (TmString s) ->
      TmNum (String.length s)

  | TmLength t1 ->
      let t1' = eval1 vctx t1 in       
      TmLength t1'

  | TmVar s ->
    getbinding vctx s

  |TmPro (TmReg l as v , s) when isval(v) -> 
    List.assoc s l 

  |TmPro (TmReg (tmr), n) ->
     List.assoc n tmr 
     

  | TmPro (TmTuple l as v , s) when isval (v) ->
    List.nth l (int_of_string s - 1)

  | TmPro (t,n) ->
    TmPro ((eval1 vctx t), n)

    | TmReg tmr ->
      let rec eval_reg = function
        | [] -> raise NoRuleApplies
        | (s, tm) :: t when isval tm ->
          if List.exists (fun (tag, _) -> tag = s) t then
            raise (Type_error "cannot create record, duplicated tag")
          else
            (s, tm) :: (eval_reg t)
        | (s, tm) :: t ->
          (s, eval1 vctx tm) :: t
      in TmReg (eval_reg tmr)
    
      
  | TmTuple tml ->
    let rec eval_rcd = function
      [] -> raise NoRuleApplies
      |(tm::t) when isval tm -> tm::(eval_rcd t)
      |(tm::t) -> (eval1 vctx tm)::t
    in TmTuple (eval_rcd tml)

  | TmCons(ty,h,t) when isval h -> TmCons(ty,h,(eval1 vctx t))

  | TmCons(ty,h,t) -> TmCons(ty,(eval1 vctx h),t)

  | TmHead(ty,TmCons(_,h,_)) -> h

  | TmHead (ty,t) ->
      TmHead(ty, eval1 vctx t)
  
  | TmTail(ty,TmCons(_,_,t)) -> t

  | TmTail (ty,t) ->
      TmTail(ty, eval1 vctx t)

  | TmisEmpty(ty,TmNil(_)) -> TmTrue

  | TmisEmpty(ty,TmCons(_,_,_)) -> TmFalse

  | TmisEmpty(ty,t) -> TmisEmpty(ty,eval1 vctx t)

  | _ ->
      raise NoRuleApplies      
;;

let apply_ctx vctx tm = 
  List.fold_left (fun t x -> subst x (getbinding vctx x)t) tm (free_vars tm)
;;

let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    eval vctx tm'
  with
    NoRuleApplies -> apply_ctx vctx tm
;;

let rec string_to_char_list s =
  match s with
  | "" -> []
  | n -> List.of_seq (String.to_seq n)

let contar_y_agregar_tabuladores cadena =
  let rec contar_tabuladores_rec cadena contador resultado =
    match cadena with
    | [] -> resultado
    | '\n' :: t ->
        let tabuladores = String.make contador ' ' in
        contar_tabuladores_rec t (contador + 2) (resultado ^ "\n" ^ tabuladores)
    | h :: t -> contar_tabuladores_rec t contador (resultado ^ Char.escaped h)
  in
  contar_tabuladores_rec (string_to_char_list cadena) 2 ""
;;

let execute (vctx, tctx) = function
  Eval tm ->
    let tyTm = typeof tctx tm in 
    let tm' = eval vctx tm in 
    print_endline (contar_y_agregar_tabuladores ("- : " ^ string_of_ty tyTm ^ " =\n " ^ string_of_term tm'));
    (vctx,tctx)

  | Bind (s,tm) ->  
    let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in 
        print_endline(contar_y_agregar_tabuladores (s ^ " : " ^ string_of_ty tyTm ^ " =\n" ^ string_of_term tm'));
        (addbinding vctx s tm', addbinding tctx s tyTm)

  | BindTy (s, ty) ->
    print_endline(s ^ " : " ^ string_of_ty ty);
    (addbinding vctx s (TmType ty), addbinding tctx s ty)

