open PyreAst.Concrete
open Base

let default_hash_size = 64

module State = struct
  type t =
    { methods_count : int
    ; method_idx : (string, int) Hashtbl.t
    }
  [@@deriving sexp_of]

  let default =
    { methods_count = 0
    ; method_idx = Hashtbl.create ~size:default_hash_size (module String)
    }
  ;;
end

let rec py_module s m =
  let open Module in
  let process_statement (s, body) stmt =
    let s, stmt = statement s stmt in
    s, stmt :: body
  in
  let s, body = Base.List.fold m.body ~init:(s, []) ~f:process_statement in
  let body = List.rev body in
  s, Module.make_t ~body ~type_ignores:m.type_ignores ()

and exec_stmt (s, body) stmt =
  let s, stmt = statement s stmt in
  s, stmt :: body

and exec_stmts s body =
  let s, body = Base.List.fold body ~init:(s, []) ~f:exec_stmt in
  s, List.rev body

and statement (s : State.t) stmt =
  let open Statement in
  let open State in
  match stmt with
  | ClassDef { body; location; name; bases; keywords; decorator_list } ->
    let s, body = exec_stmts s body in
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
    Hashtbl.update s.method_idx (Identifier.to_string name) ~f:(fun _ -> s.methods_count);
    let s = { s with methods_count = s.methods_count + 1 } in
    let s, body = exec_stmts s body in
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
  | _ -> s, stmt
;;
