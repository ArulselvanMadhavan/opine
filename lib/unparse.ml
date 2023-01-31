open PyreAst.Concrete
(* open Concrete.Expression *)
(* open Concrete.Constant *)
(* open Concrete.BinaryOperator *)
(* open Concrete.Statement *)

module Precedence = struct
  type t =
    | Named_Expr
    | Tuple
    | Yield
    | Test
    | Or
    | And
    | Not
    | Cmp
    | Expr
    | Bor
    | Bxor
    | Band
    | Shift
    | Arith
    | Term
    | Factor
    | Power
    | Await
    | Atom
  [@@deriving compare, sexp, hash]
end

let default_hash_size = 64

let expr_precedences : (Expression.t, Precedence.t) Base.Hashtbl.t =
  Base.Hashtbl.create ~size:default_hash_size (module Expression)
;;

module State = struct
  open Base

  type t =
    { source : string
    ; indent : int
    ; expr_precedences : (Expression.t, Precedence.t) Hashtbl.t
    }
  [@@deriving make]

  let default = { source = ""; indent = 0; expr_precedences }
  let ( ++= ) s str = { s with source = s.source ^ str }

  let block s f =
    let s = { s with source = s.source ^ ":" } in
    let s = { s with indent = s.indent + 1 } in
    let s = f s in
    { s with indent = s.indent - 1 }
  ;;

  let delimit s start end_ f =
    let s = s ++= start in
    let s = f s in
    s ++= end_
  ;;
end

let noop = "noop"
let noop_state (s : State.t) = State.(s ++= noop)
let maybe_newline (s : State.t) = if Base.String.is_empty s.source then "" else "\n"

let fill (s : State.t) text =
  let newline = maybe_newline s in
  let indents = Base.List.init s.indent ~f:(fun _ -> "    ") in
  let indents = Base.String.concat indents in
  State.(s ++= (newline ^ indents ^ text))
;;

let bin_op o =
  let open BinaryOperator in
  match o with
  | Add -> "+"
  | Mult -> "*"
  | _ -> noop
;;

let repr str =
  let buf = Buffer.create (String.length str + 10) in
  Buffer.add_char buf '\'';
  Buffer.add_char buf '"';
  Base.String.iter str ~f:(fun c ->
    if c = '\'' then Buffer.add_char buf '\\';
    Buffer.add_char buf c);
  Buffer.add_char buf '"';
  Buffer.add_char buf '\'';
  Buffer.contents buf
;;

(* let result = Py.Run.simple_string "print('Hello, world!')" in *)
(* if result then "succ" else "Fail" *)
(* let _builtins = Py.Eval.get_builtins () in *)
(* let repr_python = Py.Dict.find_string builtins "repr" in *)
(* let repr_python = Py.Callable.to_function repr_python in *)
(* Py.String.to_string (repr_python [|Py.String.of_string str|]) *)

let constant c =
  let open Constant in
  match c with
  | Integer i -> Int.to_string i
  | String s -> repr s
  | True | False -> Sexplib0.Sexp.to_string (Constant.sexp_of_t c)
  | _ ->
    Printf.printf "Hit noop";
    noop
;;

let single_quotes = [| "'"; "\"" |]
let multi_quotes = [| "\"\"\""; "'''" |]
let all_quotes = Array.concat [ single_quotes; multi_quotes ]

let multi_quotes_ht =
  let ht = Base.Hash_set.create ~size:2 (module Base.String) in
  Array.iter (Base.Hash_set.add ht) multi_quotes;
  ht
;;

let _str_literal_helper str escape_special_whitespace quote_types =
  let is_printable c =
    let c = Char.code c in
    32 <= c && c < 127
  in
  let escape_char c =
    if (not escape_special_whitespace) && (c == '\n' || c == '\t')
    then c
    else if c == '\\' || not (is_printable c)
    then (
      let buf = Buffer.create 1 in
      Uutf.Buffer.add_utf_8 buf (Uchar.of_char c);
      Buffer.nth buf 0)
    else c
  in
  let escaped_string = Base.String.map str ~f:escape_char in

  let possible_quotes = quote_types in
  let possible_quotes =
    if Base.String.contains escaped_string '\n' then multi_quotes else possible_quotes
  in
  let possible_quotes =
    Base.Array.filter possible_quotes ~f:(fun pq ->
      Option.is_none (Base.String.substr_index escaped_string ~pattern:pq))
  in
  if Base.Array.is_empty possible_quotes
  then (
    let str = repr str in
    let sq = String.get str 0 in
    let quote =
      Base.Array.find_exn quote_types ~f:(fun q ->
        Option.is_some (Base.String.find q ~f:(fun qq -> sq = qq)))
    in
    Base.String.drop_prefix str 1, [| quote |])
  else if not (Base.String.is_empty escaped_string)
  then (
    Base.Array.sort possible_quotes ~compare:(fun l r ->
      let eq = String.get escaped_string ((String.length escaped_string) - 1) in
      let leq = String.get l 0 = eq in
      let req = String.get r 0 = eq in
      Base.Bool.compare leq req);
    let pq = possible_quotes.(0) in
    let pq = String.get pq 0 in
    let lc = String.get escaped_string ((String.length escaped_string) - 1) in
    if pq == lc
    then (
      let escaped_string =
        Base.String.drop_suffix escaped_string 1
        ^ "\\"
        ^ Base.String.suffix escaped_string 1
      in
      escaped_string, possible_quotes)
    else escaped_string, possible_quotes)
  else escaped_string, possible_quotes
;;

let _write_str_avoiding_backslashes s doc ~quote_types =
  let open State in
  let doc, quote_types = _str_literal_helper doc false quote_types in
  let quote_type = quote_types.(0) in
  s ++= Printf.sprintf "%s%s%s" quote_type doc quote_type
;;

let get_docstring node =
  let open Statement in
  match node with
  | AsyncFunctionDef { body; _ } | FunctionDef { body; _ } | ClassDef { body; _ } ->
    if List.length body = 0
    then None
    else (
      match List.hd body with
      | Expr { value; _ } ->
        (match value with
         | Constant { value; _ } ->
           (match value with
            | String s -> Some s
            | _ -> None)
         | _ -> None)
      | _ -> None)
  | _ -> None
;;

let write_docstring s = function
  | Some doc ->
    let s = fill s "" in
    _write_str_avoiding_backslashes s doc ~quote_types:multi_quotes
  | None -> s
;;

let rec arg (s : State.t) a =
  let open Argument in
  let open State in
  let id = Identifier.to_string a.identifier in
  let s = s ++= id in
  Option.fold ~none:s ~some:(fun a -> expr (s ++= ":") a) a.annotation

and arguments s xs =
  let open Arguments in
  let open State in
  let all_args = xs.posonlyargs @ xs.args in
  let defaults = Array.of_list xs.defaults in
  let defaults_start_idx = List.length all_args - List.length xs.defaults in
  let pos_only_idx = List.length xs.posonlyargs - 1 in
  let process_arg idx (s : State.t) a =
    (* Check comma *)
    let s = if idx > 0 then s ++= ", " else s in
    let s = arg s a in
    (* Check default arg *)
    let s =
      if idx >= defaults_start_idx
      then (
        let d_idx = idx - defaults_start_idx in
        let d_expr = defaults.(d_idx) in
        let s = s ++= "=" in
        expr s d_expr)
      else s
    in
    (* check pos only *)
    if idx == pos_only_idx then s ++= ", /" else s
  in
  Base.List.foldi all_args ~init:s ~f:process_arg

and import_alias ia =
  let open ImportAlias in
  Identifier.to_string ia.name
  ^ Option.fold ~none:"" ~some:(fun asn -> " as " ^ Identifier.to_string asn) ia.asname

and process_names names =
  let aliases = Base.List.map names ~f:import_alias in
  Base.String.concat ~sep:", " aliases

and function_helper s ~decorator_list ~name ~args ~body ~def =
  let open State in
  let s = s ++= maybe_newline s in
  let s = Base.List.fold ~init:s ~f:(fun s e -> expr (fill s "@") e) decorator_list in
  let s = fill s (def ^ " " ^ Identifier.to_string name) in
  let s = delimit s "(" ")" (fun s -> arguments s args) in
  let process_body s = Base.List.fold ~init:s ~f:statement body in
  let s = block s process_body in
  s

and statement (s : State.t) stmt =
  let open Statement in
  let open State in
  match stmt with
  | Import { names; _ } ->
    let s = fill s "import " in
    s ++= process_names names
  | ImportFrom { names; level; module_; _ } ->
    (* self.write("." * (node.level or 0)) *)
    let dots = Base.List.init level ~f:(fun _ -> ".") in
    let dots = Base.String.concat dots in
    let mod_name = Option.fold ~none:"" ~some:Identifier.to_string module_ in
    fill s "from " ++= (dots ^ mod_name ^ " import " ^ process_names names)
  | ClassDef { name; decorator_list; bases; keywords; body; _ } as node ->
    (* decorator *)
    let s =
      Base.List.fold
        ~init:(s ++= maybe_newline s)
        decorator_list
        ~f:(fun s dec -> expr (fill s "@") dec)
    in
    (* class *)
    let s = fill s ("class " ^ Identifier.to_string name) in
    (* parens *)
    let is_paren = List.length bases > 0 || List.length keywords > 0 in
    let s = if is_paren then s ++= "(" else s in
    (* bases *)
    let s =
      Base.List.foldi ~init:s bases ~f:(fun idx s b ->
        let s = if idx > 0 then s ++= ", " else s in
        expr s b)
    in
    (* keywords *)
    let s = if is_paren then s ++= ")" else s in
    (* body *)
    let docstring = get_docstring node in
    let s = write_docstring s docstring in
    let body = Option.fold ~some:(fun _ -> List.tl body) ~none:body docstring in
    let process_body s = Base.List.fold ~init:s ~f:statement body in
    let s = block s process_body in
    s
  | FunctionDef { decorator_list; name; args; body; _ } ->
    function_helper s ~decorator_list ~name ~args ~body ~def:"def"
  | Assign { targets; value; _ } ->
    let s = fill s "" in
    let process_target s tgt =
      Base.Hashtbl.update s.expr_precedences tgt ~f:(fun _ -> Precedence.Tuple);
      let s = expr s tgt in
      s ++= " = "
    in
    let s = Base.List.fold targets ~init:s ~f:process_target in
    let s = expr s value in
    s
  | Expr { value; _ } -> expr s value
  | _x -> noop_state s

and expr (s : State.t) e =
  let open Base in
  let open State in
  match e with
  | BinOp { left; right; op; _ } ->
    let s = expr s left in
    let s = s ++= bin_op op in
    expr s right
  | Constant { value; _ } -> s ++= constant value
  | Lambda { args; body; _ } as node ->
    let node_prec =
      Option.value (Hashtbl.find s.expr_precedences node) ~default:Precedence.Test
    in
    let eval_prec = Precedence.Test in
    let is_paren = Precedence.(compare node_prec eval_prec) > 0 in
    let s = if is_paren then s ++= "(" else s in
    let s = s ++= ("lambda" ^ " ") in
    let s = arguments s args in
    let s = s ++= ": " in
    (* check body *)
    Hashtbl.update s.expr_precedences body ~f:(fun _ -> Precedence.Test);
    let s = expr s body in
    if is_paren then s ++= ")" else s
  | Name { id; _ } -> s ++= Identifier.to_string id
  | Attribute { value; attr; _ } ->
    let open Expression in
    Hashtbl.update s.expr_precedences value ~f:(fun _ -> Precedence.Atom);
    let s = expr s value in
    let handle_special_case s = function
      | Constant { value; _ } ->
        (match value with
         | Constant.Integer _ -> s ++= " "
         | _ -> s)
      | _ -> s
    in
    let s = handle_special_case s value in
    let s = s ++= "." in
    let s = s ++= Identifier.to_string attr in
    s
  | _ -> noop_state s

and py_module s m =
  let open Module in
  let process_statement s stmt = statement s stmt in
  Base.List.fold m.body ~init:s ~f:process_statement
;;
