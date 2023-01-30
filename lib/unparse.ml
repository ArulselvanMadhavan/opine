open PyreAst.Concrete
(* open Concrete.Expression *)
(* open Concrete.Constant *)
(* open Concrete.BinaryOperator *)
(* open Concrete.Statement *)

let noop = "noop"

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

let require_parens node_prec eval_prec content =
  if node_prec > eval_prec then "(" ^ content ^ ")" else content
;;

let maybe_newline (s:State.t) =
  if Base.String.is_empty s.source then "" else "\n"
let fill (s : State.t) text =
  let newline = maybe_newline s in
  let indents = Base.List.init s.indent ~f:(fun _ -> "    ") in
  let indents = Base.String.concat indents in
  newline ^ indents ^ text
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

let rec arg s a =
  let open Argument in
  let id = Identifier.to_string a.identifier in
  let annot = Option.fold ~none:"" ~some:(fun a -> ":" ^ expr s a) a.annotation in
  id ^ annot

and arguments s xs =
  let open Arguments in
  let all_args = xs.posonlyargs @ xs.args in
  let defaults = Array.of_list xs.defaults in
  let defaults_start_idx = List.length all_args - List.length xs.defaults in
  let pos_only_idx = List.length xs.posonlyargs - 1 in
  let acc = ref "" in
  let process_arg idx a =
    (* Check comma *)
    if idx > 0 then acc := !acc ^ ", ";
    acc := !acc ^ arg s a;
    (* Check default arg *)
    if idx >= defaults_start_idx
    then (
      let d_idx = idx - defaults_start_idx in
      let d_expr = defaults.(d_idx) in
      acc := !acc ^ "=" ^ expr s d_expr);
    (* check pos only *)
    if idx == pos_only_idx then acc := !acc ^ ", /"
  in
  Base.List.iteri all_args ~f:process_arg;
  !acc

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
  | Import { names ; _ } ->
    fill s "import " ^ process_names names 
  | ImportFrom {names; level;module_;_} ->
    (* self.write("." * (node.level or 0)) *)
    let dots = Base.List.init level ~f:(fun _ -> ".") in
    let dots = Base.String.concat dots in
    let mod_name = Option.fold ~none:"" ~some:(Identifier.to_string) module_ in
    fill s "from " ^ dots ^ mod_name ^ " import " ^ process_names names
  | ClassDef {name;decorator_list;_} ->
    (* let newline = maybe_newline s in *)
    let s = Base.List.fold ~init:s decorator_list ~f:(fun s dec ->
        let at = fill s "@" in
        let result = expr {s with source = s.source ^ at } dec in
        {s with source = result}
      ) in
    fill s ("class " ^ Identifier.to_string name)
  | _x -> noop

and expr (s : State.t) e =
  let open Base in
  match e with
  | BinOp { left; right; op; _ } -> expr s left ^ " " ^ bin_op op ^ " " ^ expr s right
  | Constant { value; _ } -> constant value
  | Lambda { args; body; _ } as node ->
    let content = "lambda" ^ " " ^ arguments s args ^ ": " in
    (* check body *)
    Hashtbl.update s.expr_precedences body ~f:(fun _ -> Precedence.Test);
    let content = content ^ expr s body in
    (* check parens *)
    let node_prec =
      Option.value (Hashtbl.find s.expr_precedences node) ~default:Precedence.Test
    in
    let eval_prec = Precedence.Test in
    require_parens node_prec eval_prec content
  | Name { id; _ } -> Identifier.to_string id
  | _ -> noop

and py_module s m =
  let open Module in
  let acc = ref "" in
  let process_statement stmt =
    acc := !acc ^ statement { s with source = !acc } stmt
  in
  Base.List.iter m.body ~f:process_statement;
  !acc
;;

(* and py_module mod = *)
(*   let open Concrete.Module in *)
(*   "" *)
