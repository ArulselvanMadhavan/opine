open PyreAst.Concrete
(* open Concrete.Expression *)
(* open Concrete.Constant *)
(* open Concrete.BinaryOperator *)
(* open Concrete.Statement *)

let default_hash_size = 64

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
  [@@deriving compare, sexp, hash, enum]

  let next t = of_enum (to_enum t + 1)
end

let unary_precedences =
  let ht = Base.Hashtbl.create ~size:default_hash_size (module UnaryOperator) in
  Base.Hashtbl.update ht (UnaryOperator.make_not_of_t ()) ~f:(fun _ -> Precedence.Not);
  Base.Hashtbl.update ht (UnaryOperator.make_invert_of_t ()) ~f:(fun _ ->
    Precedence.Factor);
  Base.Hashtbl.update ht (UnaryOperator.make_uadd_of_t ()) ~f:(fun _ -> Precedence.Factor);
  Base.Hashtbl.update ht (UnaryOperator.make_usub_of_t ()) ~f:(fun _ -> Precedence.Factor);
  ht
;;

let binop_precedences =
  let ht = Base.Hashtbl.create ~size:default_hash_size (module BinaryOperator) in
  Base.Hashtbl.update ht (BinaryOperator.make_add_of_t ()) ~f:(fun _ -> Precedence.Arith);
  Base.Hashtbl.update ht (BinaryOperator.make_sub_of_t ()) ~f:(fun _ -> Precedence.Arith);
  Base.Hashtbl.update ht (BinaryOperator.make_mult_of_t ()) ~f:(fun _ -> Precedence.Term);
  Base.Hashtbl.update ht (BinaryOperator.make_matmult_of_t ()) ~f:(fun _ ->
    Precedence.Term);
  Base.Hashtbl.update ht (BinaryOperator.make_div_of_t ()) ~f:(fun _ -> Precedence.Term);
  Base.Hashtbl.update ht (BinaryOperator.make_mod_of_t ()) ~f:(fun _ -> Precedence.Term);
  Base.Hashtbl.update ht (BinaryOperator.make_lshift_of_t ()) ~f:(fun _ ->
    Precedence.Shift);
  Base.Hashtbl.update ht (BinaryOperator.make_rshift_of_t ()) ~f:(fun _ ->
    Precedence.Shift);
  Base.Hashtbl.update ht (BinaryOperator.make_bitor_of_t ()) ~f:(fun _ -> Precedence.Bor);
  Base.Hashtbl.update ht (BinaryOperator.make_bitxor_of_t ()) ~f:(fun _ ->
    Precedence.Bxor);
  Base.Hashtbl.update ht (BinaryOperator.make_bitand_of_t ()) ~f:(fun _ ->
    Precedence.Band);
  Base.Hashtbl.update ht (BinaryOperator.make_floordiv_of_t ()) ~f:(fun _ ->
    Precedence.Term);
  Base.Hashtbl.update ht (BinaryOperator.make_pow_of_t ()) ~f:(fun _ -> Precedence.Power);
  ht
;;

let boolop_precedences =
  let ht = Base.Hashtbl.create ~size:default_hash_size (module BooleanOperator) in
  Base.Hashtbl.update ht (BooleanOperator.make_and_of_t ()) ~f:(fun _ -> Precedence.And);
  Base.Hashtbl.update ht (BooleanOperator.make_or_of_t ()) ~f:(fun _ -> Precedence.Or);
  ht
;;

let expr_precedences : (Expression.t, Precedence.t) Base.Hashtbl.t =
  Base.Hashtbl.create ~size:default_hash_size (module Expression)
;;

let binop_rassoc =
  let ht = Base.Hashtbl.create ~size:1 (module BinaryOperator) in
  Base.Hashtbl.update ht (BinaryOperator.make_pow_of_t ()) ~f:(fun _ -> true);
  ht
;;

module State = struct
  open Base

  type t =
    { source : string
    ; indent : int
    ; expr_precedences : (Expression.t, Precedence.t) Hashtbl.t
    ; avoid_backslashes : bool
    }
  [@@deriving make]

  let default = { source = ""; indent = 0; expr_precedences; avoid_backslashes = false }
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

  let delimit_if s start end_ condition f =
    if condition then delimit s start end_ f else f s
  ;;

  let require_parens s eval_prec node_prec =
    delimit_if s "(" ")" (Precedence.compare node_prec eval_prec > 0)
  ;;

  let buffered f = f default
end

exception CauseWithoutException of string
exception UnexpectedNode of string
exception ValueError of string

let noop = "noop"
let noop_state (s : State.t) = State.(s ++= noop)
let maybe_newline (s : State.t) = if Base.String.is_empty s.source then "" else "\n"

let fill (s : State.t) text =
  let newline = maybe_newline s in
  let indents = Base.List.init s.indent ~f:(fun _ -> "    ") in
  let indents = Base.String.concat indents in
  State.(s ++= (newline ^ indents ^ text))
;;

let comp_op o =
  let open ComparisonOperator in
  match o with
  | NotEq -> "!="
  | IsNot -> "is not"
  | Eq -> "=="
  | _ -> noop
;;

let unary_op o =
  let open UnaryOperator in
  match o with
  | Not -> "not"
  | Invert -> "~"
  | UAdd -> "+"
  | USub -> "-"
;;

let bool_op o =
  let open BooleanOperator in
  match o with
  | And -> "and"
  | Or -> "or"
;;

let bin_op o =
  let open BinaryOperator in
  match o with
  | Add -> "+"
  | Mult -> "*"
  | FloorDiv -> "//"
  | Pow -> "**"
  | _ -> noop
;;

let repr str =
  (* let buf = Buffer.create (String.length str + 10) in *)
  (* Buffer.add_char buf '\''; *)
  (* Buffer.add_char buf '"'; *)
  (* Base.String.iter str ~f:(fun c -> *)
  (*   if c = '\'' then Buffer.add_char buf '\\'; *)
  (*   Buffer.add_char buf c); *)
  (* Buffer.add_char buf '"'; *)
  (* Buffer.add_char buf '\''; *)
  (* Buffer.contents buf *)
  str
;;

let constant c =
  let open Constant in
  match c with
  | Integer i -> Int.to_string i
  | String s -> "\"" ^ repr s ^ "\""
  | Float f -> Float.to_string f
  | True | False | None -> Sexplib0.Sexp.to_string (Constant.sexp_of_t c)
  | _ -> noop
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
      let eq = String.get escaped_string (String.length escaped_string - 1) in
      let leq = String.get l 0 = eq in
      let req = String.get r 0 = eq in
      Base.Bool.compare leq req);
    let pq = possible_quotes.(0) in
    let pq = String.get pq 0 in
    let lc = String.get escaped_string (String.length escaped_string - 1) in
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

let _write_str_avoiding_backslashes ?(quote_types = all_quotes) s doc =
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
  Option.fold ~none:s ~some:(fun a -> expr (s ++= ": ") a) a.annotation

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
  let s = Base.List.foldi all_args ~init:s ~f:process_arg in
  (* varargs, or bare '*' if no varargs but keyword-only arguments present *)
  let s =
    if Option.is_some xs.vararg || List.length xs.kwonlyargs > 0
    then (
      let s = if List.length all_args > 0 then s ++= ", " else s in
      let s = s ++= "*" in
      let s = Base.Option.fold xs.vararg ~init:s ~f:arg in
      s)
    else s
  in
  (* kwonlyargs *)
  let zipped = Base.List.zip_exn xs.kwonlyargs xs.kw_defaults in
  let s =
    Base.List.fold zipped ~init:s ~f:(fun s (a, d) ->
      let s = s ++= ", " in
      let s = arg s a in
      Base.Option.fold ~init:s ~f:(fun s d -> expr (s ++= "=") d) d)
  in
  (* kwargs *)
  let s =
    Base.Option.fold xs.kwarg ~init:s ~f:(fun s a ->
      let s =
        if List.length all_args > 0
           || Option.is_some xs.vararg
           || List.length xs.kwonlyargs > 0
        then s ++= ", "
        else s
      in
      arg (s ++= "**") a)
  in
  s

and import_alias ia =
  let open ImportAlias in
  Identifier.to_string ia.name
  ^ Option.fold ~none:"" ~some:(fun asn -> " as " ^ Identifier.to_string asn) ia.asname

and process_names names =
  let aliases = Base.List.map names ~f:import_alias in
  Base.String.concat ~sep:", " aliases

and function_helper s node ~decorator_list ~name ~args ~body ~returns ~def =
  let open State in
  let s = s ++= maybe_newline s in
  let s = Base.List.fold ~init:s ~f:(fun s e -> expr (fill s "@") e) decorator_list in
  let s = fill s (def ^ " " ^ Identifier.to_string name) in
  let s = delimit s "(" ")" (fun s -> arguments s args) in
  let s = Base.Option.fold returns ~init:s ~f:(fun s r -> expr (s ++= " -> ") r) in
  let s = block s (docstring_and_body node body) in
  s

and docstring_and_body node body s =
  let docstring = get_docstring node in
  let s = write_docstring s docstring in
  let body = Option.fold ~some:(fun _ -> List.tl body) ~none:body docstring in
  Base.List.fold ~init:s ~f:statement body

and _write_fstring_inner s node =
  let open Expression in
  match node with
  | JoinedStr { values; _ } -> Base.List.fold values ~init:s ~f:_write_fstring_inner
  | Constant { value; _ } -> State.(s ++= constant value)
  | FormattedValue _ -> expr s node
  | _ -> raise (UnexpectedNode "fstring_inner")

and items_view s items =
  let open State in
  if List.length items == 1
  then expr s (List.hd items) ++= "," (* End with a comma *)
  else
    Base.List.foldi items ~init:s ~f:(fun idx s a ->
      expr (if idx > 0 then s ++= ", " else s) a)

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
    let s = block s (docstring_and_body node body) in
    s
  | FunctionDef { decorator_list; name; args; body; returns; _ } as node ->
    function_helper s node ~decorator_list ~name ~args ~body ~returns ~def:"def"
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
  | Expr { value; _ } ->
    let s = fill s "" in
    Base.Hashtbl.update s.expr_precedences ~f:(fun _ -> Precedence.Yield) value;
    expr s value
  | If { test; body; orelse; _ } ->
    let s = fill s "if " in
    let s = expr s test in
    let s = block s (fun s -> Base.List.fold body ~init:s ~f:statement) in
    (* Collapse nested if into elif *)
    let s = ref s in
    let orelse_list = ref orelse in
    while
      List.length !orelse_list == 1
      &&
      match List.hd !orelse_list with
      | If _ -> true
      | _ -> false
    do
      let hd = List.hd !orelse_list in
      match hd with
      | If { test; body; orelse; _ } ->
        orelse_list := orelse;
        s := expr (fill !s "elif ") test;
        s := block !s (fun s -> Base.List.fold ~init:s ~f:statement body)
      | _ -> ()
    done;
    let s = !s in
    let orelse = !orelse_list in
    if List.length orelse > 0 (* No more if statements *)
    then (
      let s = fill s "else" in
      block s (fun s -> Base.List.fold orelse ~init:s ~f:statement))
    else s
  | Raise { exc; cause; location } ->
    let s = fill s "raise" in
    (* Node without exception but cause *)
    if Option.is_none exc && Option.is_some cause
    then
      raise
        (CauseWithoutException (Sexplib0.Sexp.to_string (Location.sexp_of_t location)));
    let s = State.(s ++= " ") in
    let s = Base.Option.fold ~init:s ~f:expr exc in
    let s = Base.Option.fold ~init:s ~f:(fun s c -> expr (s ++= " from ") c) cause in
    s
  | Return { value; _ } ->
    Base.Option.fold value ~init:(fill s "return") ~f:(fun s a -> expr (s ++= " ") a)
  | _x -> noop_state s

and expr (s : State.t) e =
  let open Base in
  let open State in
  match e with
  | UnaryOp { op; operand; _ } as node ->
    let op_prec = Base.Hashtbl.find_exn unary_precedences op in
    let node_prec =
      Option.value (Base.Hashtbl.find s.expr_precedences node) ~default:Precedence.Test
    in
    let f s =
      let s = s ++= unary_op op in
      let s =
        if Precedence.(compare op_prec Precedence.Factor) <> 0 then s ++= " " else s
      in
      Base.Hashtbl.update s.expr_precedences operand ~f:(fun _ -> op_prec);
      expr s operand
    in
    require_parens s op_prec node_prec f
  | BinOp { left; right; op; _ } as node ->
    let op_prec = Base.Hashtbl.find_exn binop_precedences op in
    let node_prec =
      Option.value (Base.Hashtbl.find s.expr_precedences node) ~default:Precedence.Test
    in
    let f s =
      let rassoc_opt = Base.Hashtbl.find binop_rassoc op in
      let l_prec, r_prec =
        Base.Option.fold
          ~init:(op_prec, Option.value_exn (Precedence.next op_prec))
          ~f:(fun _ _ -> Option.value_exn (Precedence.next op_prec), op_prec)
          rassoc_opt
      in
      Base.Hashtbl.update s.expr_precedences left ~f:(fun _ -> l_prec);
      let s = expr s left in
      let s = s ++= (" " ^ bin_op op ^ " ") in
      Base.Hashtbl.update s.expr_precedences right ~f:(fun _ -> r_prec);
      expr s right
    in
    require_parens s op_prec node_prec f
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
  | Call { func; args; keywords; _ } ->
    (* self.set_precedence(_Precedence.ATOM, node.func) *)
    Hashtbl.update s.expr_precedences func ~f:(fun _ -> Precedence.Atom);
    let s = expr s func in
    let process_call s =
      let s =
        Base.List.foldi
          ~init:s
          ~f:(fun idx s a -> expr (if idx > 0 then s ++= ", " else s) a)
          args
      in
      Base.List.foldi keywords ~init:s ~f:(fun idx s a ->
        let s = if idx > 0 || List.length args > 0 then s ++= ", " else s in
        let s =
          if Option.is_none a.arg
          then s ++= "**"
          else s ++= (Identifier.to_string (Option.value_exn a.arg) ^ "=")
        in
        expr s a.value)
    in
    let s = delimit s "(" ")" process_call in
    s
  | Compare { left; ops; comparators; _ } ->
    let node_prec =
      Option.value (Hashtbl.find s.expr_precedences left) ~default:Precedence.Test
    in
    let f s =
      Base.Hashtbl.update s.expr_precedences left ~f:(fun _ ->
        Base.Option.value_exn (Precedence.next Precedence.Cmp));
      let s = expr s left in
      let zipped = Base.List.zip_exn ops comparators in
      Base.List.fold zipped ~init:s ~f:(fun s oe ->
        let o, e = oe in
        let s = State.(s ++= (" " ^ comp_op o ^ " ")) in
        expr s e)
    in
    require_parens s Precedence.Cmp node_prec f
  | JoinedStr { values; _ } as node ->
    let s = State.(s ++= "f") in
    if s.avoid_backslashes
    then (
      let f s = _write_fstring_inner s node in
      let buffer = buffered f in
      _write_str_avoiding_backslashes s buffer.source)
    else (
      let f e =
        let buffer = buffered (fun s -> _write_fstring_inner s e) in
        ( buffer.source
        , match e with
          | Constant _ -> true
          | _ -> false )
      in
      let fstring_parts = Base.List.map values ~f in
      let new_fstring_parts, quote_types =
        Base.List.fold
          fstring_parts
          ~init:([], all_quotes)
          ~f:(fun acc (str, is_constant) ->
          let new_fstring_parts, quote_types = acc in
          let new_fstring, quote_types =
            _str_literal_helper str is_constant quote_types
          in
          new_fstring :: new_fstring_parts, quote_types)
      in
      let new_fstring_parts = List.rev new_fstring_parts in
      let quote_type = quote_types.(0) in
      s ++= (quote_type ^ String.concat new_fstring_parts ^ quote_type))
  | FormattedValue { value; conversion; format_spec; _ } ->
    let unparse_inner inner =
      let s = { State.default with avoid_backslashes = true } in
      Base.Hashtbl.update s.expr_precedences inner ~f:(fun _ ->
        Option.value_exn (Precedence.next Precedence.Test));
      expr s inner
    in
    let f s =
      let e = unparse_inner value in
      let src = e.source in
      if Base.String.contains src '\\'
      then raise (ValueError "Unable to avoid backslash in f-string expression part");
      let s = if String.equal (Base.String.prefix src 1) "{" then s ++= " " else s in
      let s = State.(s ++= src) in
      let s =
        if not (Int.equal conversion (-1))
        then s ++= String.of_char (Char.of_int_exn conversion)
        else s
      in
      Base.Option.fold format_spec ~init:s ~f:(fun s fmt ->
        _write_fstring_inner (s ++= ":") fmt)
    in
    delimit s "{" "}" f
  | Subscript { value; slice; _ } ->
    Base.Hashtbl.update s.expr_precedences ~f:(fun _ -> Precedence.Atom) value;
    let s = expr s value in
    let process_slice s =
      match slice with
      | Tuple { elts; _ } -> if List.length elts > 0 then items_view s elts else s
      | _ -> expr s slice
    in
    delimit s "[" "]" process_slice
  | Tuple { elts; _ } as node ->
    let f s = items_view s elts in
    let node_prec = Base.Hashtbl.find s.expr_precedences node in
    let node_prec = Option.value node_prec ~default:Precedence.Test in
    let condition =
      List.length elts = 0 || Precedence.compare node_prec Precedence.Tuple > 0
    in
    delimit_if s "(" ")" condition f
  | List { elts; _ } ->
    let f s =
      Base.List.foldi elts ~init:s ~f:(fun idx s e ->
        if idx > 0 then expr (s ++= ", ") e else expr s e)
    in
    delimit s "[" "]" f
  | BoolOp { op; values; _ } as node ->
    let op_prec = Base.Hashtbl.find boolop_precedences op in
    let op_prec = Option.value op_prec ~default:Precedence.Test in
    let node_prec = Base.Hashtbl.find s.expr_precedences node in
    let node_prec = Option.value node_prec ~default:Precedence.Test in
    let f s =
      let increasing_level_traverse s op_prec e =
        let op_prec = Option.value_exn (Precedence.next op_prec) in
        Base.Hashtbl.update s.expr_precedences e ~f:(fun _ -> op_prec);
        expr s e, op_prec
      in
      let s, _ =
        Base.List.foldi values ~init:(s, op_prec) ~f:(fun idx (s, op_prec) e ->
          let s = if idx > 0 then s ++= (" " ^ bool_op op ^ " ") else s in
          increasing_level_traverse s op_prec e)
      in
      s
    in
    require_parens s op_prec node_prec f
  | Starred { value; _ } ->
    let s = s ++= "*" in
    Base.Hashtbl.set ~key:value ~data:Precedence.Expr s.expr_precedences;
    expr s value
  | _ -> noop_state s

and py_module s m =
  let open Module in
  let process_statement s stmt = statement s stmt in
  Base.List.fold m.body ~init:s ~f:process_statement
;;
