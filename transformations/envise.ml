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
  let qattn_id = Identifier.make_t "qattn" () in
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
            ~id:qattn_id
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
    let return_ =
      let value =
        Expression.make_name_of_t
          ~location
          ~id:qattn_id
          ~ctx:(ExpressionContext.make_load_of_t ())
          ()
      in
      Statement.make_return_of_t ~location ~value ()
    in
    [ qattn; return_ ]
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

let add_envise_args func =
  let open Statement in
  match func with
  | FunctionDef {args;body;decorator_list;returns;type_comment;name;_} ->
    let location = default_loc in
    let identifier = (Identifier.make_t "run_on_envise" ()) in
    let annotation = Expression.make_name_of_t ~location ~id:(Identifier.make_t "bool" ()) ~ctx:(ExpressionContext.make_load_of_t ()) () in
    let run_on_envise = Argument.make_t ~location ~identifier ~annotation () in
    let false_def = Expression.make_constant_of_t ~location ~value:(Constant.make_false_of_t ()) ~kind:"bool" () in
    let args = Arguments.make_t ~args:(List.append args.args [run_on_envise]) ~posonlyargs:args.posonlyargs ~kw_defaults:args.kw_defaults ~kwonlyargs:args.kwonlyargs ~defaults:(List.append args.defaults [false_def]) ?vararg:args.vararg ?kwarg:args.kwarg () in
    Statement.make_functiondef_of_t ~location ~args ~body ~decorator_list ~name ?returns ?type_comment ()
  | _ -> func
    
(* body *)
let rec py_module (s : State.t) m =
  let open Module in
  let body = Base.List.map m.body ~f:(fun stmt -> statement s stmt) in
  make_t ~body ~type_ignores:m.type_ignores ()

and statement s stmt =
  match stmt with
  | ClassDef { body; location; name; bases; keywords; decorator_list } ->
    let load_ctx = ExpressionContext.make_load_of_t () in
    let body = add_from_float s body in
    let body = Array.of_list body in
    let init_idx = Hashtbl.find_exn s.method_idx "__init__" in
    let init_func = body.(init_idx) in
    let init_func = add_envise_args init_func in
    body.(init_idx) <- init_func;
    let body = Array.to_list body in
    let base =
      Expression.make_name_of_t ~location:default_loc ~id:name ~ctx:load_ctx ()
    in
    let name = Identifier.make_t ("Idiom" ^ Identifier.to_string name) () in
    let stmt =
      Statement.make_classdef_of_t
        ~location
        ~name
        ~bases:(base :: bases)
        ~keywords
        ~decorator_list
        ~body
        ()
    in
    stmt
  | _ -> stmt
;;
