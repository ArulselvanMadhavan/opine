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
let layer_repeat = "layer_repeat"
let quantized_backward = "quantized_backward"
let backend = "backend"
let softmax_type = "softmax_type"

let envise_params_arr =
  EnviseParams.
    [| { default_value = "DEFAULT_LAYER_REPEAT"
       ; param_type = "int"
       ; param_name = layer_repeat
       }
     ; { default_value = "DEFAULT_QUANTIZED_BACKWARD"
       ; param_type = "bool"
       ; param_name = quantized_backward
       }
     ; { default_value = "DEFAULT_RUN_ON_ENVISE"
       ; param_type = "bool"
       ; param_name = run_on_envise
       }
     ; { default_value = "None"; param_type = "Optional[Envise]"; param_name = backend }
     ; { default_value = "DEFAULT_SOFTMAX_TYPE"
       ; param_type = "str"
       ; param_name = softmax_type
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
let torch_id = Identifier.make_t "torch" ()
let bmm_id = Identifier.make_t "bmm" ()
let init_id = Identifier.make_t "__init__" ()
let fwd_id = Identifier.make_t "forward" ()
let super_id = Identifier.make_t "super" ()
let bool_id = Identifier.make_t "bool" ()
let envise_id = Identifier.make_t "Envise" ()
let assign_param_id = Identifier.make_t "assign_param" ()
let prot_param_id p = Identifier.make_t ("_" ^ p) ()
let arg identifier = Argument.make_t ~location:default_loc ~identifier ()
let self_arg = arg self_id

let name_l name =
  Expression.make_name_of_t
    ~location:default_loc
    ~id:name
    ~ctx:(ExpressionContext.make_load_of_t ())
    ()
;;

let func_call ~args ~func ?(keywords = []) () =
  Expression.make_call_of_t ~location:default_loc ~func:(name_l func) ~args ~keywords ()
;;

let super_call cls_name = func_call ~args:[ cls_name; name_l self_id ] ~func:super_id ()

let property_decorator =
  Expression.make_name_of_t
    ~location:default_loc
    ~id:(Identifier.make_t "property" ())
    ~ctx:(ExpressionContext.make_load_of_t ())
    ()
;;

let attr_l ?(ctx = ExpressionContext.make_load_of_t ()) ~value ~attr () =
  Expression.make_attribute_of_t ~location:default_loc ~attr ~value ~ctx ()
;;

let attr_call ~name ~attr ~args ?(keywords = []) () =
  Expression.make_call_of_t
    ~location:default_loc
    ~func:(attr_l ~value:(name_l name) ~attr ())
    ~args
    ~keywords
    ()
;;

let method_call ~attr ~args ?(keywords = []) () =
  Expression.make_call_of_t
    ~location:default_loc
    ~func:(attr_l ~value:(name_l self_id) ~attr ())
    ~args
    ~keywords
    ()
;;

let super_attr ~attr cls_name = attr_l ~value:(super_call cls_name) ~attr ()

let super_method_call ~cls_name ~attr ~args =
  let args_to_expr args =
    let open Argument in
    Base.List.map args ~f:(fun a -> name_l a.identifier)
  in
  let location = default_loc in
  let super_method = super_attr ~attr cls_name in
  Expression.make_call_of_t ~location ~func:super_method ~args:(args_to_expr args) ()
;;

let self_attr ~attr =
  let value = name_l self_id in
  attr_l ~value ~attr ~ctx:(ExpressionContext.make_store_of_t ()) ()
;;

let return_ ~expr = Statement.make_return_of_t ~location:default_loc ~value:expr ()
let constant ~value = Expression.make_constant_of_t ~location:default_loc ~value ()

let assign_param src dst param members =
  Base.List.map members ~f:(fun member ->
    let value = Constant.make_string_of_t member in
    let param_attr v = attr_l ~value:(name_l v) ~attr:(Identifier.make_t param ()) () in
    let call_args = [ param_attr src; param_attr dst; constant ~value ] in
    let call = func_call ~func:assign_param_id ~args:call_args () in
    Statement.make_expr_of_t ~location:default_loc ~value:call ())
;;

let not_run_on_envise =
  let ep_idx = Hashtbl.find_exn envise_params run_on_envise in
  let ep = envise_params_arr.(ep_idx) in
  let test = self_attr ~attr:(Identifier.make_t ep.param_name ()) in
  Expression.make_unaryop_of_t
    ~location:default_loc
    ~op:(UnaryOperator.make_not_of_t ())
    ~operand:test
    ()
;;

let add_method method_params body =
  let location = default_loc in
  let args = List.map method_params ~f:(fun i -> Identifier.make_t i ()) in
  let args = self_id :: args in
  let args = List.map args ~f:arg in
  let args = Arguments.make_t ~args () in
  Statement.make_functiondef_of_t ~location ~name:bmm_id ~args ~body ()
;;

let add_bmm =
  let method_params = [ "weight"; "value" ] in
  let args = List.map method_params ~f:(fun i -> Identifier.make_t i () |> name_l) in
  let if_body = [ return_ ~expr:(attr_call ~name:torch_id ~attr:bmm_id ~args ()) ] in
  let if_not_envise =
    Statement.make_if_of_t ~location:default_loc ~test:not_run_on_envise ~body:if_body ()
  in
  let args = self_attr ~attr:(Identifier.make_t backend ()) :: args in
  let bmm_call = func_call ~args ~func:bmm_id () in
  add_method method_params [ if_not_envise; return_ ~expr:bmm_call ]
;;

let add_from_float s body =
  let open Statement in
  let name = Identifier.make_t "from_float" () in
  let location = default_loc in
  let cls_id = Identifier.make_t "cls" () in
  let mod_id = Identifier.make_t "mod" () in
  let cls_arg = Argument.make_t ~location ~identifier:cls_id () in
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
      let targets = [ name_l qattn_id ] in
      let ctx = ExpressionContext.make_load_of_t () in
      let value = name_l mod_id in
      let partial_attr = Expression.make_attribute_of_t ~location ~value ~ctx in
      (* Do this only for args for now *)
      let keyword_args Argument.{ identifier; _ } =
        let value = partial_attr ~attr:identifier () in
        if Hash_set.mem s.members (Identifier.to_string identifier)
        then Keyword.make_t ~location ~arg:identifier ~value ()
        else (
          let value = name_l (Identifier.make_t "" ()) in
          Keyword.make_t ~location ~arg:identifier ~value ())
      in
      let args = Base.Option.value_exn (List.tl init_args.args) in
      let keywords = Base.List.map args ~f:keyword_args in
      let value = func_call ~func:cls_id ~args:[] ~keywords () in
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
    let assign_params = ref [] in
    Hash_set.iter s.linear_params ~f:(fun p ->
      assign_params
        := assign_param qattn_id mod_id p [ "weight"; "bias" ] :: !assign_params);
    let assign_params = List.concat !assign_params in
    List.append (qattn :: assign_params) [ return_ ]
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

let handle_fwd ~cls_name ~func =
  let open Statement in
  match func with
  | FunctionDef { name; args; decorator_list; returns; type_comment; body; _ } ->
    let location = default_loc in
    let super_fwd =
      super_method_call ~cls_name ~attr:fwd_id ~args:(List.tl_exn args.args)
    in
    let if_stmt =
      Statement.make_if_of_t
        ~location
        ~test:not_run_on_envise
        ~body:[ return_ ~expr:super_fwd ]
        ()
    in
    let body = List.append [ if_stmt (* return_ ~expr:super_fwd *) ] body in
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

let transform_bmm stmt =
  let open Statement in
  let open Expression in
  match stmt with
  | Assign { targets; value = Call { args; _ }; location; type_comment } ->
    let value = method_call ~attr:bmm_id ~args () in
    make_assign_of_t ~value ~location ~targets ?type_comment ()
  | _ -> stmt
;;

let transform_bmms func bmm_idxs =
  let open Statement in
  match func with
  | FunctionDef { body; location; args; decorator_list; name; returns; type_comment } ->
    let body = Array.of_list body in
    List.iter bmm_idxs ~f:(fun i ->
      let bmm_stmt = body.(i) in
      let bmm_stmt = transform_bmm bmm_stmt in
      body.(i) <- bmm_stmt);
    let body = Array.to_list body in
    make_functiondef_of_t
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

let handle_init ~cls_name ~func =
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
        let stmt =
          if String.equal param_name backend
          then (
            (* Backend needs deepcopy *)
            let deepcopy =
              func_call
                ~args:[ name_l identifier ]
                ~func:(Identifier.make_t "deepcopy" ())
                ()
            in
            let envise_backend = func_call ~func:envise_id ~args:[] () in
            let value =
              Expression.make_boolop_of_t
                ~location
                ~op:(BooleanOperator.make_or_of_t ())
                ~values:[ deepcopy; envise_backend ]
                ()
            in
            Statement.make_assign_of_t ~location ~targets ~value ())
          else Statement.make_assign_of_t ~location ~targets ~value ()
        in
        envise_arg, envise_def, stmt)
    in
    let envise_info = Array.to_list envise_info in
    (* End param *)
    (* super.method_call *)
    let super_init =
      super_method_call ~cls_name ~attr:init_id ~args:(List.tl_exn orig_args)
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

let ep_getters =
  Array.map envise_params_arr ~f:(fun ep -> add_getters ep.param_name) |> Array.to_list
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
    let name = Identifier.make_t ("Idiom" ^ Identifier.to_string name) () in
    let cls_name = name_l name in
    (* First do transformations that doesn't affect statement idxs *)
    (* transform bmms *)
    Hashtbl.iteri s.bmms ~f:(fun ~key ~data ->
      let method_idx = Hashtbl.find_exn s.method_idx key in
      let mth = body.(method_idx) in
      let bmm_idx = data in
      let mth = transform_bmms mth bmm_idx in
      body.(method_idx) <- mth);    
    (* Update init method *)
    let init_idx = Hashtbl.find_exn s.method_idx "__init__" in
    let func = body.(init_idx) in
    let init_func = handle_init ~cls_name ~func in
    body.(init_idx) <- init_func;
    (* Update forward method *)
    let fwd_idx = Hashtbl.find_exn s.method_idx "forward" in
    let func = body.(fwd_idx) in
    let fwd_func = handle_fwd ~cls_name ~func in
    body.(fwd_idx) <- fwd_func;
    (* Add additional methods *)
    let body = Array.to_list body in
    let immut_base = name_l (Identifier.make_t "ImmutableDtypeMixin" ()) in
    let body = List.append body ep_getters in
    let body = add_from_float s body in
    let body = List.append body [ add_bmm ] in
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
