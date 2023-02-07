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
let dim_id = Identifier.make_t "dim" ()
let cls_id = Identifier.make_t "cls" ()
let mod_id = Identifier.make_t "mod" ()
let qconfig_id = Identifier.make_t "qconfig" ()
let softmax_id = Identifier.make_t "Softmax" ()
let qnn_id = Identifier.make_t "qnn" ()
let torch_id = Identifier.make_t "torch" ()
let bmm_id = Identifier.make_t "bmm" ()
let apply_id = Identifier.make_t "apply" ()
let squeeze_id = Identifier.make_t "squeeze" ()
let output_id = Identifier.make_t "output" ()
let tiled_gemm_id = Identifier.make_t "TiledGEMM" ()
let reshape_id = Identifier.make_t "reshape" ()
let tiled_nd_gemm_id = Identifier.make_t "tiled_nd_gemm" ()
let matmul_id = Identifier.make_t "matmul" ()
let init_id = Identifier.make_t "__init__" ()
let fwd_id = Identifier.make_t "forward" ()
let super_id = Identifier.make_t "super" ()
let bool_id = Identifier.make_t "bool" ()
let envise_id = Identifier.make_t "Envise" ()
let backend_id = Identifier.make_t backend ()
let quantized_backward_id = Identifier.make_t quantized_backward ()
let layer_repeat_id = Identifier.make_t layer_repeat ()
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

let softmax_keyword_args =
  let roe = envise_params_arr.(Hashtbl.find_exn envise_params run_on_envise) in
  let bk = envise_params_arr.(Hashtbl.find_exn envise_params backend) in
  let st = envise_params_arr.(Hashtbl.find_exn envise_params softmax_type) in
  let args = [ roe; bk; st ] in
  let keyword ep =
    let ep_id = Identifier.make_t EnviseParams.(ep.param_name) () in
    Keyword.make_t ~location:default_loc ~arg:ep_id ~value:(self_attr ~attr:ep_id) ()
  in
  Base.List.map args ~f:keyword
;;

let keywords_envise =
  Array.map
    (Array.filter envise_params_arr ~f:(fun ep -> String.(ep.param_name <> softmax_type)))
    ~f:(fun ep ->
      let arg = Identifier.make_t EnviseParams.(ep.param_name) () in
      let value =
        attr_l ~value:(attr_l ~value:(name_l mod_id) ~attr:qconfig_id ()) ~attr:arg ()
      in
      Keyword.make_t ~location:default_loc ~arg ~value ())
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

let add_method ~name method_params body =
  let location = default_loc in
  let args = List.map method_params ~f:(fun i -> Identifier.make_t i ()) in
  let args = self_id :: args in
  let args = List.map args ~f:arg in
  let args = Arguments.make_t ~args () in
  Statement.make_functiondef_of_t ~location ~name ~args ~body ()
;;

let add_tiled_nd_gemm =
  let open Statement in
  let open Expression in
  let method_params = [ "weight"; "value" ] in
  let args = List.map method_params ~f:(fun i -> Identifier.make_t i () |> name_l) in
  let method_args = args in
  let if_body = [ return_ ~expr:(attr_call ~name:torch_id ~attr:matmul_id ~args ()) ] in
  let if_not_envise =
    Statement.make_if_of_t ~location:default_loc ~test:not_run_on_envise ~body:if_body ()
  in
  let orig_value_id = Identifier.make_t "orig_value" () in
  let value_id = Identifier.make_t "value" () in
  let target = name_l orig_value_id in
  let value = name_l value_id in
  let orig_value = make_assign_of_t ~location:default_loc ~targets:[ target ] ~value () in
  let comparators =
    [ make_constant_of_t ~location:default_loc ~value:(Constant.make_integer_of_t 1) () ]
  in
  let left =
    attr_l ~value:(name_l orig_value_id) ~attr:(Identifier.make_t "ndim" ()) ()
  in
  let is_1d =
    make_compare_of_t
      ~location:default_loc
      ~ops:[ ComparisonOperator.make_eq_of_t () ]
      ~comparators
      ~left
      ()
  in
  let int1 = constant ~value:(Constant.make_integer_of_t 1) in
  let neg1 =
    make_unaryop_of_t
      ~location:default_loc
      ~op:(UnaryOperator.make_usub_of_t ())
      ~operand:int1
      ()
  in
  let args = [ neg1; int1 ] in
  let reshape_call = attr_call ~name:value_id ~attr:reshape_id ~args () in
  let body =
    [ make_assign_of_t ~location:default_loc ~targets:[ value ] ~value:reshape_call () ]
  in
  let check1d = make_if_of_t ~location:default_loc ~test:is_1d ~body () in
  let args = self_attr ~attr:backend_id :: method_args in
  let args =
    List.append
      args
      [ self_attr ~attr:quantized_backward_id; self_attr ~attr:layer_repeat_id ]
  in
  let value = attr_call ~name:tiled_gemm_id ~attr:apply_id ~args () in
  let output_assign =
    make_assign_of_t ~location:default_loc ~targets:[ name_l output_id ] ~value ()
  in
  let value = attr_call ~name:output_id ~attr:squeeze_id ~args:[ neg1 ] () in
  let output1d_assign =
    make_assign_of_t ~location:default_loc ~targets:[ name_l output_id ] ~value ()
  in
  let output_1d =
    make_if_of_t ~location:default_loc ~test:is_1d ~body:[ output1d_assign ] ()
  in
  add_method
    ~name:tiled_nd_gemm_id
    method_params
    [ if_not_envise
    ; orig_value
    ; check1d
    ; output_assign
    ; output_1d
    ; return_ ~expr:(name_l output_id)
    ]
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
  add_method ~name:bmm_id method_params [ if_not_envise; return_ ~expr:bmm_call ]
;;

let add_from_float s body =
  let open Statement in
  let name = Identifier.make_t "from_float" () in
  let location = default_loc in
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
      (* Add envise args *)
      let keywords = List.append keywords (Array.to_list keywords_envise) in
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
    let linear_params = Hashtbl.find_exn s.linear_params "__init__" in
    List.iter linear_params ~f:(fun (p, _) ->
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

let place_if if_stmt body =
  let open Statement in
  match body with
  (* Check docstring *)
  | (Expr { value = Constant { value = String _; _ }; _ } as e) :: xs ->
    e :: if_stmt :: xs
  | _ -> if_stmt :: body
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
    let body = place_if if_stmt body in
    (* let body = List.append [ if_stmt ] body in *)
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

let rec transform_expr ~f e =
  let open Expression in
  match e with
  | Call { func; location; args; keywords } ->
    let func = transform_expr ~f func in
    make_call_of_t ~location ~args ~func ~keywords ()
  | Attribute { value = Name _; _ } as node -> f node (* end of recursion *)
  | _ -> e
;;

let transform_stmt ~f stmt =
  let open Statement in
  match stmt with
  | Assign { value; location; targets; type_comment } ->
    let value = transform_expr ~f value in
    make_assign_of_t ~location ~value ~targets ?type_comment ()
  | _ -> stmt
;;

let transform_method_body ~f func bmm_idxs =
  let open Statement in
  match func with
  | FunctionDef { body; location; args; decorator_list; name; returns; type_comment } ->
    let body = Array.of_list body in
    List.iter bmm_idxs ~f:(fun i ->
      let bmm_stmt = body.(i) in
      let bmm_stmt = transform_stmt ~f bmm_stmt in
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

let rec transform_smax stmt =
  let open Statement in
  match stmt with
  | Assign { value; location; targets; type_comment } ->
    let value = smax_expr value in
    make_assign_of_t ~location ~targets ?type_comment ~value ()
  | If { body; orelse; test; location } ->
    let body = Base.List.map body ~f:transform_smax in
    let orelse = Base.List.map orelse ~f:transform_smax in
    make_if_of_t ~location ~test ~body ~orelse ()
  | _ -> stmt

and smax_expr e =
  let open Expression in
  match e with
  | Call { func = Attribute { attr; _ }; args; keywords; _ }
    when String.equal (Identifier.to_string attr) "softmax" ->
    (* We need a dim arg because it's required in qnn.Softmax but torch doesn't require it*)
    let dim_value =
      if Int.equal (List.length args) 2
      then List.hd_exn (List.tl_exn args)
      else
        List.hd_exn
          (List.filter_map keywords ~f:(fun k ->
             if String.equal (Identifier.to_string @@ Option.value_exn k.arg) "dim"
             then Some k.value
             else None))
    in
    let dim_kw = Keyword.make_t ~location:default_loc ~arg:dim_id ~value:dim_value () in
    let keywords = dim_kw :: softmax_keyword_args in
    let qnn_smax_call = attr_call ~name:qnn_id ~attr:softmax_id ~args:[] ~keywords () in
    make_call_of_t ~location:default_loc ~func:qnn_smax_call ~args:[ List.hd_exn args ] ()
  | Call
      { func = Attribute { value; attr; location; ctx }; args; keywords; location = loc1 }
    ->
    let value = smax_expr value in
    let attr = make_attribute_of_t ~location ~attr ~value ~ctx () in
    make_call_of_t ~func:attr ~args ~keywords ~location:loc1 ()
  | Call { func; _ } -> smax_expr func
  | _ -> e
;;

let transform_smaxs func smax_idxs =
  let open Statement in
  match func with
  | FunctionDef { location; args; body; decorator_list; name; returns; type_comment } ->
    let body = Array.of_list body in
    List.iter smax_idxs ~f:(fun i ->
      let smax_stmt = body.(i) in
      let smax_stmt = transform_smax smax_stmt in
      body.(i) <- smax_stmt);
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
    (* let linear_params = List.map (Hash_set.to_list s.linear_params) ~f: *)
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

let transformations s =
  let open State in
  [| s.bmms, transform_method_body ~f:(fun _attr_node -> self_attr ~attr:bmm_id)
   ; s.softmaxs, transform_smaxs
   ; ( s.matmuls
     , transform_method_body ~f:(fun _attr_node -> self_attr ~attr:tiled_nd_gemm_id) )
  |]
;;

let apply_transformations s body =
  let tfs = transformations s in
  Array.iter tfs ~f:(fun (ht, tfm) ->
    Hashtbl.iteri ht ~f:(fun ~key ~data ->
      let method_idx = Hashtbl.find_exn s.method_idx key in
      let mth = body.(method_idx) in
      let bmm_idx = data in
      let mth = tfm mth bmm_idx in
      body.(method_idx) <- mth));
  body
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
    let body = apply_transformations s body in
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
    let body = List.append body [ add_bmm; add_tiled_nd_gemm ] in
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
