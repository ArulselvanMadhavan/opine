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
end

let noop = "noop"
let ( ++= ) (s : State.t) str = { s with source = s.source ^ str }
let noop_state (s : State.t) = s ++= noop
let maybe_newline (s : State.t) = if Base.String.is_empty s.source then "" else "\n"

let fill (s : State.t) text =
  let newline = maybe_newline s in
  let indents = Base.List.init s.indent ~f:(fun _ -> "    ") in
  let indents = Base.String.concat indents in
  s ++= (newline ^ indents ^ text)
;;

let bin_op o =
  let open BinaryOperator in
  match o with
  | Add -> "+"
  | Mult -> "*"
  | _ -> noop
;;

let constant c =
  let open Constant in
  match c with
  | Integer i -> Int.to_string i
  | _ -> noop
;;

let rec arg (s : State.t) a =
  let open Argument in
  let id = Identifier.to_string a.identifier in
  let s = { s with source = s.source ^ id } in
  Option.fold
    ~none:s
    ~some:(fun a -> expr { s with source = s.source ^ ":" } a)
    a.annotation

and arguments s xs =
  let open Arguments in
  let all_args = xs.posonlyargs @ xs.args in
  let defaults = Array.of_list xs.defaults in
  let defaults_start_idx = List.length all_args - List.length xs.defaults in
  let pos_only_idx = List.length xs.posonlyargs - 1 in
  let process_arg idx (s : State.t) a =
    (* Check comma *)
    let s = if idx > 0 then { s with source = s.source ^ ", " } else s in
    let s = arg s a in
    (* Check default arg *)
    let s =
      if idx >= defaults_start_idx
      then (
        let d_idx = idx - defaults_start_idx in
        let d_expr = defaults.(d_idx) in
        let s = { s with source = s.source ^ "=" } in
        expr s d_expr)
      else s
    in
    (* check pos only *)
    if idx == pos_only_idx then { s with source = s.source ^ ", /" } else s
  in
  Base.List.foldi all_args ~init:s ~f:process_arg

and import_alias ia =
  let open ImportAlias in
  Identifier.to_string ia.name
  ^ Option.fold ~none:"" ~some:(fun asn -> " as " ^ Identifier.to_string asn) ia.asname

and process_names names =
  let aliases = Base.List.map names ~f:import_alias in
  Base.String.concat ~sep:", " aliases

and statement (s : State.t) stmt =
  let open Statement in
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
  | ClassDef { name; decorator_list; bases; _ } ->
    (* decorator *)
    let s =
      Base.List.fold
        ~init:(s ++= maybe_newline s)
        decorator_list
        ~f:(fun s dec -> expr (fill s "@") dec)
    in
    (* class *)
    let s = fill s ("class " ^ Identifier.to_string name) in
    (* bases *)
    let s =
      Base.List.foldi ~init:s bases ~f:(fun idx s b ->
        let s = if idx > 0 then s ++= ", " else s in
        expr s b)
    in
    (* keywords *)
    s
  | _x -> noop_state s

and expr (s : State.t) e =
  let open Base in
  match e with
  | BinOp { left; right; op; _ } ->
    let left = expr s left in
    let right = expr s right in
    { s with source = left.source ^ " " ^ bin_op op ^ " " ^ right.source }
  | Constant { value; _ } -> { s with source = s.source ^ constant value }
  | Lambda { args; body; _ } as node ->
    let node_prec =
      Option.value (Hashtbl.find s.expr_precedences node) ~default:Precedence.Test
    in
    let eval_prec = Precedence.Test in
    let is_paren = Precedence.(compare node_prec eval_prec) > 0 in
    let s = if is_paren then { s with source = s.source ^ "(" } else s in
    let s = { s with source = s.source ^ "lambda" ^ " " } in
    let s = arguments s args in
    let s = { s with source = s.source ^ ": " } in
    (* check body *)
    Hashtbl.update s.expr_precedences body ~f:(fun _ -> Precedence.Test);
    let s = expr s body in
    if is_paren then { s with source = s.source ^ ")" } else s
  | Name { id; _ } ->
    { s with source = s.source ^ Identifier.to_string id } (* | Attribute {value;_} -> *)
  | _ -> noop_state s

and py_module s m =
  let open Module in
  let process_statement s stmt = statement s stmt in
  Base.List.fold m.body ~init:s ~f:process_statement
;;

(* and py_module mod = *)
(*   let open Concrete.Module in *)
(*   "" *)
