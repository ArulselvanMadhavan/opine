open PyreAst.Concrete
open Base

let default_hash_size = 64

module State = struct
  type t =
    { methods_count : int
    ; method_idx : (string, int) Hashtbl.t
    ; init_args : Arguments.t option
    ; members : string Hash_set.t
    ; is_store : bool
    }
  [@@deriving sexp_of]

  let default =
    { methods_count = 0
    ; method_idx = Hashtbl.create ~size:default_hash_size (module String)
    ; init_args = None
    ; members = Hash_set.create ~size:default_hash_size (module String)
    ; is_store = false
    }
  ;;
end

let exec_elem f (s, body) stmt =
  let s, stmt = f s stmt in
  s, stmt :: body
;;

let exec_list f s body =
  let s, body = Base.List.fold body ~init:(s, []) ~f:(exec_elem f) in
  s, List.rev body
;;

let rec py_module s m =
  let open Module in
  let process_statement (s, body) stmt =
    let s, stmt = statement s stmt in
    s, stmt :: body
  in
  let s, body = Base.List.fold m.body ~init:(s, []) ~f:process_statement in
  let body = List.rev body in
  s, Module.make_t ~body ~type_ignores:m.type_ignores ()

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
            s, e)
          else s, e
        | _ -> s, e)
     | _ -> s, e)
  | _ -> s, e

and statement (s : State.t) stmt =
  let open Statement in
  let open State in
  match stmt with
  | ClassDef { body; location; name; bases; keywords; decorator_list } ->
    let s, body = exec_list statement s body in
    let stmt =
      Statement.make_classdef_of_t
        ~location
        ~name
        ~bases
        ~keywords
        ~decorator_list
        ~body
        ()
    in
    s, stmt
  | FunctionDef { body; location; name; args; decorator_list; returns; type_comment } ->
    let method_name = Identifier.to_string name in
    Hashtbl.update s.method_idx method_name ~f:(fun _ -> s.methods_count);
    let s =
      { s with
        methods_count = s.methods_count + 1
      ; init_args =
          (if String.equal method_name "__init__" then Some args else s.init_args)
      }
    in
    let s, body = exec_list statement s body in
    let partial =
      Statement.make_functiondef_of_t ~body ~location ~name ~args ~decorator_list
    in
    let stmt =
      match returns, type_comment with
      | Some returns, Some type_comment -> partial ~returns ~type_comment ()
      | None, Some type_comment -> partial ~type_comment ()
      | Some returns, None -> partial ~returns ()
      | None, None -> partial ()
    in
    s, stmt
  | Assign { location; targets; value; type_comment } ->
    let s, targets = exec_list expr s targets in
    let partial = Statement.make_assign_of_t ~location ~targets ~value in
    let stmt =
      if Option.is_some type_comment
      then partial ~type_comment:(Option.value_exn type_comment) ()
      else partial ()
    in
    s, stmt
  | _ -> s, stmt
;;
