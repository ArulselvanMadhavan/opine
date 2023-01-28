open PyreAst
open Concrete.Expression
open Concrete.Constant
open Concrete.BinaryOperator
open Concrete.Statement

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

let expr_precedences : (Concrete.Expression.t, Precedence.t) Base.Hashtbl.t =
  Base.Hashtbl.create ~size:default_hash_size (module Concrete.Expression)
;;

module State = struct
  open Base

  type t =
    { source : string list
    ; indent : int
    ; expr_precedences : (Concrete.Expression.t, Precedence.t) Hashtbl.t
    }
  [@@deriving make]

  let default = { source = []; indent = 0; expr_precedences }
end

let require_parens node_prec eval_prec content =
  if node_prec > eval_prec then "(" ^ content ^ ")" else content
;;

let bin_op = function
  | Add -> "+"
  | Mult -> "*"
  | _ -> noop
;;

let constant = function
  | Integer i -> Int.to_string i
  | _ -> noop
;;

let rec arg s a =
  let open Concrete.Argument in
  let id = Concrete.Identifier.to_string a.identifier in
  let annot = Option.fold ~none:"" ~some:(fun a -> ":" ^ expr s a) a.annotation in
  id ^ annot

and arguments s xs =
  let open Concrete.Arguments in
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

and statement = function
  | Import _ -> ""
  | _ -> noop

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
  | Name { id; _ } -> Concrete.Identifier.to_string id
  | _ -> noop

and py_module m =
  let open Concrete.Module in
  let acc = ref "" in
  let process_statement s = acc := !acc ^ "\n" ^ statement s in
  Base.List.iter m.body ~f:process_statement;
  !acc
;;

(* and py_module mod = *)
(*   let open Concrete.Module in *)
(*   "" *)
