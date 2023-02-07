open PyreAst.Concrete
open Base

let default_hash_size = 64

(* Assumptions
   1. Module has only one class; everything is contained inside that class.
*)

module LinearCallInfo = struct
  type t =
    { idx : int
    ; param : string
    ; args : Expression.t list
    ; keywords : Keyword.t list
    }
  [@@deriving sexp_of, make]
end

module State = struct
  type t =
    { methods_count : int
    ; statement_count : int
    ; method_idx : (string, int) Hashtbl.t
    ; init_args : Arguments.t option
    ; members : string Hash_set.t
    ; linear_params : (string, LinearCallInfo.t list) Hashtbl.t
    ; class_name : string
    ; bmms : (string, int list) Hashtbl.t
    ; softmaxs : (string, int list) Hashtbl.t
    ; matmuls : (string, int list) Hashtbl.t
    }
  [@@deriving sexp_of, make]

  let default =
    let method_idx = Hashtbl.create ~size:default_hash_size (module String) in
    let members = Hash_set.create ~size:default_hash_size (module String) in
    let linear_params = Hashtbl.create ~size:default_hash_size (module String) in
    let bmms = Hashtbl.create ~size:default_hash_size (module String) in
    let softmaxs = Hashtbl.create ~size:default_hash_size (module String) in
    let matmuls = Hashtbl.create ~size:default_hash_size (module String) in
    make_t
      ~methods_count:0
      ~method_idx
      ~members
      ~statement_count:0
      ~linear_params
      ~bmms
      ~softmaxs
      ~matmuls
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

let loop_until ~f xs =
  Base.List.fold_until
    xs
    ~init:false
    ~f:(fun _acc a ->
      let result = f a in
      if result then Continue_or_stop.Stop true else Continue_or_stop.Continue false)
    ~finish:(fun _ -> false)
;;

let rec search_attr ~search s =
  let open Statement in
  match s with
  | Assign { value; _ } -> search_expr ~search value
  | If { body; orelse; _ } ->
    let f = search_attr ~search in
    loop_until ~f body || loop_until ~f orelse
  | _ -> false

and search_expr ~search e =
  let open Expression in
  match e with
  | Call { func; args; _ } ->
    let f = search_expr ~search in
    search_expr ~search func || loop_until ~f args
  | Attribute { attr; _ } when String.(Identifier.to_string attr = search) -> true
  | Attribute { value; _ } -> search_expr ~search value
  | _ -> false
;;

let traverse_and_collect name body ~f ht =
  Base.List.iteri body ~f:(fun i stmt ->
    if f stmt
    then
      Hashtbl.update ht (Identifier.to_string name) ~f:(fun xs ->
        Base.Option.fold xs ~init:[ i ] ~f:(fun _ xs -> List.cons i xs))
    else ())
;;

let extract_param_name s =
  let open Statement in
  match s with
  | Assign { targets = [ Attribute { value = Name { id; _ }; attr; _ } ]; _ }
    when String.(Identifier.to_string id = "self") -> Some (Identifier.to_string attr)
  | _ -> None
;;

let extract_linear_args s =
  let open Statement in
  let open Expression in
  let extract_arg e =
    match e with
    | Call { args; keywords; _ } -> Some (args, keywords)
    | _ -> None
  in
  match s with
  | Assign { value; _ } -> extract_arg value
  | _ -> None
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
    let method_name = Identifier.to_string name in
    Hashtbl.update s.method_idx method_name ~f:(fun _ -> s.statement_count);
    let s =
      { s with
        methods_count = s.methods_count + 1
      ; init_args =
          (if String.equal method_name "__init__" then Some args else s.init_args)
      }
    in
    (* Update bmms *)
    traverse_and_collect name body ~f:(search_attr ~search:"bmm") s.bmms;
    (* Update softmax stats *)
    traverse_and_collect name body ~f:(search_attr ~search:"softmax") s.softmaxs;
    (* update matmuls *)
    traverse_and_collect name body ~f:(search_attr ~search:"matmul") s.matmuls;
    (* update linear params *)
    let linear_params =
      List.filter_mapi body ~f:(fun idx stmt ->
        let b = search_attr ~search:"Linear" stmt in
        if b
        then
          let open Option.Let_syntax in
          let%bind name = extract_param_name stmt in
          let%map args, keywords = extract_linear_args stmt in
          LinearCallInfo.make_t ~idx ~param:name ~args ~keywords ()
        else None)
    in
    Hashtbl.update s.linear_params method_name ~f:(fun _ -> linear_params);
    exec_list statement s body
  | _ -> s
;;
