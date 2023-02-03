open PyreAst.Concrete
open Stats
open Base

exception Unexpected of string

let default_pos = Position.make_t ~line:0 ~column:0 ()
let default_loc = Location.make_t ~start:default_pos ~stop:default_pos ()

let add_from_float s body =
  let open Statement in
  let name = Identifier.make_t "from_float" () in
  let location = default_loc in
  let cls = Identifier.make_t "cls" () in
  let mod_id = Identifier.make_t "mod" () in
  let cls_arg = Argument.make_t ~location ~identifier:cls () in
  let mod_arg = Argument.make_t ~location ~identifier:mod_id () in
  let args = Arguments.make_t ~args:[ cls_arg; mod_arg ] () in
  let decorator_list =
    [ Expression.make_name_of_t
        ~location
        ~id:(Identifier.make_t "classmethod" ())
        ~ctx:(ExpressionContext.make_load_of_t ())
        ()
    ]
  in
  let init_args = Base.Option.value_exn State.(s.init_args) in
  let build_body =
    let qattn =
      let cls_name =
        Expression.make_name_of_t
          ~location
          ~id:cls
          ~ctx:(ExpressionContext.make_load_of_t ())
          ()
      in
      let targets =
        [ Expression.make_name_of_t
            ~location
            ~id:(Identifier.make_t "qattn" ())
            ~ctx:(ExpressionContext.make_load_of_t ())
            ()
        ]
      in
      let ctx = ExpressionContext.make_load_of_t () in
      let value = Expression.make_name_of_t ~location ~id:mod_id ~ctx () in
      let partial_attr = Expression.make_attribute_of_t ~location ~value ~ctx in
      (* Do this only for args for now *)
      let keyword_args Argument.{ identifier; _ } =
        let value = partial_attr ~attr:identifier () in
        if Hash_set.mem s.members (Identifier.to_string identifier)
        then Keyword.make_t ~location ~arg:identifier ~value ()
        else (
          let value =
            Expression.make_name_of_t
              ~location
              ~id:(Identifier.make_t "" ())
              ~ctx:(ExpressionContext.make_load_of_t ())
              ()
          in
          Keyword.make_t ~location ~arg:identifier ~value ())
      in
      let args = Base.Option.value_exn (List.tl init_args.args) in
      let keywords = Base.List.map args ~f:keyword_args in
      let value = Expression.make_call_of_t ~location ~func:cls_name ~keywords () in
      Statement.make_assign_of_t ~location ~targets ~value ()
    in
    [ qattn ]
  in
  let from_float =
    make_functiondef_of_t
      ~location:default_loc
      ~name
      ~args
      ~decorator_list
      ~body:build_body
      ()
  in
  List.append body [ from_float ]
;;

(* body *)

let rec py_module (s : State.t) m =
  let open Module in
  let body = Base.List.map m.body ~f:(fun stmt -> statement s stmt) in
  make_t ~body ~type_ignores:m.type_ignores ()

and statement s stmt =
  match stmt with
  | ClassDef { body; location; name; bases; keywords; decorator_list } ->
    let body = add_from_float s body in
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
    stmt
  | _ -> stmt
;;
