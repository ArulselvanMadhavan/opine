open PyreAst.Concrete
open Base

let default_hash_size = 64

module State = struct
  type t =
    { methods_count : int
    ; method_idx : (string, int) Hashtbl.t
    ; init_args : Arguments.t option
    ; members : string Hash_set.t
    ; class_name : string
    }
  [@@deriving sexp_of, make]

  let default =
    let method_idx = Hashtbl.create ~size:default_hash_size (module String) in
    let members = Hash_set.create ~size:default_hash_size (module String) in
    make_t ~methods_count:0 ~method_idx ~members ()
  ;;
end

let exec_list f s xs = Base.List.fold xs ~init:s ~f

let rec py_module s m =
  let open Module in
  Base.List.fold m.body ~init:s ~f:statement

and expr s e =
  let open Expression in
  let open State in
  match e with
  | Attribute { value; attr; ctx; _ } ->
    (match ctx with
     | ExpressionContext.Store ->
       (match value with
        | Name { id; _ } ->
          if String.equal (Identifier.to_string id) "self"
          then (
            Hash_set.add s.members (Identifier.to_string attr);
            s)
          else s
        | _ -> s)
     | _ -> s)
  | _ -> s

and statement (s : State.t) stmt =
  let open Statement in
  let open State in
  match stmt with
  | ClassDef { body; name; _ } ->
    let s = { s with class_name = Identifier.to_string name } in
    exec_list statement s body
  | FunctionDef { body; name; args; _ } ->
    let method_name = Identifier.to_string name in
    Hashtbl.update s.method_idx method_name ~f:(fun _ -> s.methods_count);
    let s =
      { s with
        methods_count = s.methods_count + 1
      ; init_args =
          (if String.equal method_name "__init__" then Some args else s.init_args)
      }
    in
    exec_list statement s body
  | Assign { targets; _ } -> exec_list expr s targets
  | _ -> s
;;
