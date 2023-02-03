open PyreAst.Concrete
open Stats
open Base

exception Unexpected of string

module EnviseParams = struct
  type t =
    { default_value : string
    ; param_type : string
    ; param_name : string
    }
  [@@deriving sexp, make]
end

let run_on_envise = "run_on_envise"

let envise_params_arr =
  EnviseParams.
    [| { default_value = "DEFAULT_RUN_ON_ENVISE"
       ; param_type = "bool"
       ; param_name = run_on_envise
       }
    |]
;;

let envise_params =
  let ht = Hashtbl.create ~size:default_hash_size (module String) in
  Base.Array.iteri envise_params_arr ~f:(fun idx ep ->
    Hashtbl.update ht ep.param_name ~f:(fun _ -> idx));
  ht
;;

let default_pos = Position.make_t ~line:0 ~column:0 ()
let default_loc = Location.make_t ~start:default_pos ~stop:default_pos ()
let self_id = Identifier.make_t "self" ()
let init_id = Identifier.make_t "__init__" ()
let fwd_id = Identifier.make_t "forward" ()
let super_id = Identifier.make_t "super" ()
let bool_id = Identifier.make_t "bool" ()
let prot_param_id p = Identifier.make_t ("_" ^ p) ()
let self_arg = Argument.make_t ~location:default_loc ~identifier:self_id ()

let name_l name =
  Expression.make_name_of_t
    ~location:default_loc
    ~id:name
    ~ctx:(ExpressionContext.make_load_of_t ())
    ()
;;

let super_call orig_base =
  Expression.make_call_of_t
    ~location:default_loc
    ~func:(name_l super_id)
    ~args:[ orig_base; name_l self_id ]
    ()
;;

let property_decorator =
  Expression.make_name_of_t
    ~location:default_loc
    ~id:(Identifier.make_t "property" ())
    ~ctx:(ExpressionContext.make_load_of_t ())
    ()
;;

let super_attr ~attr orig_base =
  Expression.make_attribute_of_t
    ~location:default_loc
    ~attr
    ~value:(super_call orig_base)
    ~ctx:(ExpressionContext.make_load_of_t ())
    ()
;;

let super_method_call ~orig_base ~attr ~args =
  let args_to_expr args =
    let open Argument in
    Base.List.map args ~f:(fun a -> name_l a.identifier)
  in
  let location = default_loc in
  let super_method = super_attr ~attr orig_base in
  Expression.make_call_of_t ~location ~func:super_method ~args:(args_to_expr args) ()
;;

let self_attr ~attr =
  Expression.make_attribute_of_t
    ~location:default_loc
    ~value:
      (Expression.make_name_of_t
         ~location:default_loc
         ~id:self_id
         ~ctx:(ExpressionContext.make_load_of_t ())
         ())
    ~attr
    ~ctx:(ExpressionContext.make_store_of_t ())
    ()
;;

let return_ ~expr = Statement.make_return_of_t ~location:default_loc ~value:expr ()

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

let handle_fwd ~orig_base ~func =
  let open Statement in
  match func with
  | FunctionDef { name; args; decorator_list; returns; type_comment; _ } ->
    let location = default_loc in
    let ep_idx = Hashtbl.find_exn envise_params run_on_envise in
    let ep = envise_params_arr.(ep_idx) in
    let test = self_attr ~attr:(Identifier.make_t ep.param_name ()) in
    let super_fwd =
      super_method_call ~orig_base ~attr:fwd_id ~args:(List.tl_exn args.args)
    in
    let if_stmt =
      Statement.make_if_of_t ~location ~test ~body:[ return_ ~expr:super_fwd ] ()
    in
    let body = [ if_stmt; return_ ~expr:super_fwd ] in
    Statement.make_functiondef_of_t
      ~body
      ~location
      ~name
      ~args
      ~decorator_list
      ?returns
      ?type_comment
      ()
  | _ -> func
;;

let handle_init ~orig_base ~func =
  let open Statement in
  match func with
  | FunctionDef { args; decorator_list; returns; type_comment; name; _ } ->
    let location = default_loc in
    let orig_args = args.args in
    let envise_info =
      Array.map envise_params_arr ~f:(fun ep ->
        let param_name, param_value, param_type =
          EnviseParams.(ep.param_name, ep.default_value, ep.param_type)
        in
        let identifier = Identifier.make_t param_name () in
        let annotation =
          Expression.make_name_of_t
            ~location
            ~id:(Identifier.make_t param_type ())
            ~ctx:(ExpressionContext.make_load_of_t ())
            ()
        in
        let envise_arg = Argument.make_t ~location ~identifier ~annotation () in
        let envise_def =
          Expression.make_name_of_t
            ~location
            ~id:(Identifier.make_t param_value ())
            ~ctx:(ExpressionContext.make_load_of_t ())
            ()
        in
        let targets = [ self_attr ~attr:(prot_param_id param_name) ] in
        let value =
          Expression.make_name_of_t
            ~location
            ~id:(Identifier.make_t param_value ())
            ~ctx:(ExpressionContext.make_load_of_t ())
            ()
        in
        envise_arg, envise_def, Statement.make_assign_of_t ~location ~targets ~value ())
    in
    let envise_info = Array.to_list envise_info in
    (* End param *)
    (* super.method_call *)
    let super_init =
      super_method_call ~orig_base ~attr:init_id ~args:(List.tl_exn orig_args)
    in
    let super_init = Statement.make_expr_of_t ~location ~value:super_init () in
    let envise_args = List.map envise_info ~f:(fun (a, _, _) -> a) in
    let envise_defaults = List.map envise_info ~f:(fun (_, d, _) -> d) in
    let envise_stmts = List.map envise_info ~f:(fun (_, _, s) -> s) in
    let body = super_init :: envise_stmts in
    let args =
      Arguments.make_t
        ~args:(List.append args.args envise_args)
        ~posonlyargs:args.posonlyargs
        ~kw_defaults:args.kw_defaults
        ~kwonlyargs:args.kwonlyargs
        ~defaults:(List.append args.defaults envise_defaults)
        ?vararg:args.vararg
        ?kwarg:args.kwarg
        ()
    in
    Statement.make_functiondef_of_t
      ~location
      ~args
      ~body
      ~decorator_list
      ~name
      ?returns
      ?type_comment
      ()
  | _ -> func
;;

let add_getters param_name =
  let location = default_loc in
  let decorator_list = [ property_decorator ] in
  let args = Arguments.make_t ~args:[ self_arg ] () in
  let body =
    [ Statement.make_return_of_t
        ~location
        ~value:(self_attr ~attr:(prot_param_id param_name))
        ()
    ]
  in
  Statement.make_functiondef_of_t
    ~location
    ~name:(Identifier.make_t param_name ())
    ~decorator_list
    ~args
    ~body
    ()
;;

(* body *)
let rec py_module (s : State.t) m =
  let open Module in
  let body = Base.List.map m.body ~f:(fun stmt -> statement s stmt) in
  make_t ~body ~type_ignores:m.type_ignores ()

and statement s stmt =
  match stmt with
  | ClassDef { body; location; name; keywords; decorator_list; _ } ->
    let body = Array.of_list body in
    let orig_base = name_l name in
    (* Update init method *)
    let init_idx = Hashtbl.find_exn s.method_idx "__init__" in
    let func = body.(init_idx) in
    let init_func = handle_init ~orig_base ~func in
    body.(init_idx) <- init_func;
    (* Update forward method *)
    let fwd_idx = Hashtbl.find_exn s.method_idx "forward" in
    let func = body.(fwd_idx) in
    let fwd_func = handle_fwd ~orig_base ~func in
    body.(fwd_idx) <- fwd_func;
    (* Add additional methods *)
    let body = Array.to_list body in
    let immut_base = name_l (Identifier.make_t "ImmutableDtypeMixin" ()) in
    let name = Identifier.make_t ("Idiom" ^ Identifier.to_string name) () in
    let getter = add_getters "run_on_envise" in
    let body = List.append body [ getter ] in
    let body = add_from_float s body in
    let stmt =
      Statement.make_classdef_of_t
        ~location
        ~name
        ~bases:[ immut_base; orig_base ]
        ~keywords
        ~decorator_list
        ~body
        ()
    in
    stmt
  | _ -> stmt
;;
