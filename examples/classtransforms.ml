open PyreAst.Concrete
open Base
open Stats

let self_id = Identifier.make_t "self" ()
let bmm_id = Identifier.make_t "bmm" ()
let prot_param_id p = Identifier.make_t ("_" ^ p) ()
let setter_id = Identifier.make_t "setter" ()
let default_pos = Position.make_t ~line:0 ~column:0 ()
let default_loc = Location.make_t ~start:default_pos ~stop:default_pos ()
let arg identifier = Argument.make_t ~location:default_loc ~identifier ()
let self_arg = arg self_id

let name_l name =
  Expression.make_name_of_t
    ~location:default_loc
    ~id:name
    ~ctx:(ExpressionContext.make_load_of_t ())
    ()
;;

let attr_l ?(ctx = ExpressionContext.make_load_of_t ()) ~value ~attr () =
  Expression.make_attribute_of_t ~location:default_loc ~attr ~value ~ctx ()
;;

let self_attr ~attr =
  let value = name_l self_id in
  attr_l ~value ~attr ~ctx:(ExpressionContext.make_store_of_t ()) ()
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

let transformations s =
  let open State in
  [| s.bmms, transform_method_body ~f:(fun _attr_node -> self_attr ~attr:bmm_id) |]
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

let property_decorator =
  Expression.make_name_of_t
    ~location:default_loc
    ~id:(Identifier.make_t "property" ())
    ~ctx:(ExpressionContext.make_load_of_t ())
    ()
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

let param_setter_decorator eparam = attr_l ~value:(name_l eparam) ~attr:setter_id ()

let add_setters param_name (linear_params : LinearCallInfo.t list) =
  let location = default_loc in
  let decorator_list = [ param_setter_decorator param_name ] in
  let args =
    Arguments.make_t
      ~args:[ self_arg; Argument.make_t ~location:default_loc ~identifier:param_name () ]
      ()
  in
  let rhs = name_l param_name in
  let self_assign =
    Statement.make_assign_of_t
      ~location:default_loc
      ~targets:[ self_attr ~attr:(prot_param_id (Identifier.to_string param_name)) ]
      ~value:rhs
      ()
  in
  let assigns =
    List.map linear_params ~f:(fun lp ->
      let value = self_attr ~attr:(Identifier.make_t lp.param ()) in
      let targets = [ attr_l ~attr:param_name ~value () ] in
      Statement.make_assign_of_t ~location:default_loc ~targets ~value:rhs ())
  in
  let body = self_assign :: assigns in
  Statement.make_functiondef_of_t
    ~location
    ~name:param_name
    ~decorator_list
    ~args
    ~body
    ()
;;

let return_ ~expr = Statement.make_return_of_t ~location:default_loc ~value:expr ()

let attr_call ~name ~attr ~args ?(keywords = []) () =
  Expression.make_call_of_t
    ~location:default_loc
    ~func:(attr_l ~value:(name_l name) ~attr ())
    ~args
    ~keywords
    ()
;;

let rec py_module (s : State.t) m =
  let open Module in
  let body = Base.List.map m.body ~f:(fun stmt -> statement s stmt) in
  make_t ~body ~type_ignores:m.type_ignores ()

and statement s stmt =
  match stmt with
  | ClassDef { body; location; name; keywords; decorator_list; bases } ->
    let body = Array.of_list body in
    let body = apply_transformations s body in
    let body = Array.to_list body in
    let members_list = Hash_set.to_list s.members in
    let getters = List.map members_list ~f:(fun mm -> add_getters mm) in
    let setters =
      List.map members_list ~f:(fun mm -> add_setters (Identifier.make_t mm ()) [])
    in
    let body = List.append body (List.concat [ getters; setters ]) in
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
