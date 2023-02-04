open PyreAst.Concrete
open Base

let default_hash_size = 64

(* Assumptions
   1. Module has only one class; everything is contained inside that class.
*)
module State = struct
  type t =
    { methods_count : int
    ; statement_count : int
    ; method_idx : (string, int) Hashtbl.t
    ; init_args : Arguments.t option
    ; members : string Hash_set.t
    ; linear_params : string Hash_set.t
    ; class_name : string
    ; bmms : (string, int list) Hashtbl.t
    }
  [@@deriving sexp_of, make]

  let default =
    let method_idx = Hashtbl.create ~size:default_hash_size (module String) in
    let members = Hash_set.create ~size:default_hash_size (module String) in
    let linear_params = Hash_set.create ~size:default_hash_size (module String) in
    let bmms = Hashtbl.create ~size:default_hash_size (module String) in
    make_t
      ~methods_count:0
      ~method_idx
      ~members
      ~statement_count:0
      ~linear_params
      ~bmms
      ()
  ;;
end

let exec_list f s xs = Base.List.fold xs ~init:s ~f

let is_self_attr e =
  let open Expression in
  let open ExpressionContext in
  match e with
  | Attribute { value = Name { id; _ }; attr; ctx = Store; _ }
    when String.equal (Identifier.to_string id) "self" -> Some (Identifier.to_string attr)
  | _ -> None
;;

let is_bmm s =
  let open Statement in
  let open Expression in
  match s with
  | Assign { value = Call { func = Attribute { value = Name { id; _ }; attr; _ }; _ }; _ }
    ->
    String.equal (Identifier.to_string id) "torch"
    && String.equal (Identifier.to_string attr) "bmm"
  | _ -> false
;;

let rec py_module s m =
  let open Module in
  Base.List.fold m.body ~init:s ~f:statement

and statement (s : State.t) stmt =
  let open Statement in
  let open State in
  match stmt with
  | ClassDef { body; name; _ } ->
    let s = { s with class_name = Identifier.to_string name } in
    Base.List.fold body ~init:s ~f:(fun s stmt ->
      let s = statement s stmt in
      { s with statement_count = s.statement_count + 1 })
  | FunctionDef { body; name; args; _ } ->
    Hashtbl.update s.method_idx (Identifier.to_string name) ~f:(fun _ ->
      s.statement_count);
    let method_name = Identifier.to_string name in
    let s =
      { s with
        methods_count = s.methods_count + 1
      ; init_args =
          (if String.equal method_name "__init__" then Some args else s.init_args)
      }
    in
    Base.List.iteri body ~f:(fun i stmt ->
      if is_bmm stmt
      then
        Hashtbl.update s.bmms (Identifier.to_string name) ~f:(fun xs ->
          Base.Option.fold xs ~init:[ i ] ~f:(fun _ xs -> List.cons i xs))
      else ());
    exec_list statement s body
  | Assign { targets; value; _ } ->
    let open Expression in
    let self_params = Base.List.map targets ~f:is_self_attr in
    let self_params = Base.List.filter_opt self_params in
    Base.List.iter self_params ~f:(fun p -> Hash_set.add s.members p);
    (match value with
     | Call { func = Attribute { value = Name { id; _ }; attr; _ }; _ }
       when String.equal (Identifier.to_string id) "nn"
            && String.equal (Identifier.to_string attr) "Linear" ->
       let tgt = List.hd targets in
       let f s tgt =
         let f s p =
           Hash_set.add s.linear_params p;
           s
         in
         Base.Option.fold (is_self_attr tgt) ~init:s ~f
       in
       Base.Option.fold tgt ~init:s ~f
     | _ -> s)
  | _ -> s
;;
