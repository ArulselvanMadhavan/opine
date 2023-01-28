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
end

let default_hash_size = 64

let expr_precedences =
  Base.Hashtbl.create ~size:default_hash_size (module Concrete.Expression)
;;

type state =
  { source : string list
  ; indent : int
  ; expr_precedences : (Concrete.Expression.t, Precedence.t) Base.Hashtbl.t
  }

let bin_op = function
  | Add -> "+"
  | Mult -> "*"
  | _ -> noop
;;

let constant = function
  | Integer i -> Int.to_string i
  | _ -> noop
;;

let rec arg a =
  let open Concrete.Argument in
  let id = Concrete.Identifier.to_string a.identifier in
  let annot = Option.fold ~none:"" ~some:(fun a -> ":" ^ expr a) a.annotation in
  id ^ annot

and arguments xs =
  let open Concrete.Arguments in
  let all_args = xs.posonlyargs @ xs.args in
  let defaults = Array.of_list xs.defaults in
  let defaults_start_idx = List.length all_args - List.length xs.defaults in
  let pos_only_idx = List.length xs.posonlyargs - 1 in
  let acc = ref "" in
  let process_arg idx a =
    (* Check comma *)
    if idx > 0 then acc := !acc ^ ", ";
    acc := !acc ^ arg a;
    (* Check default arg *)
    if idx >= defaults_start_idx
    then (
      let d_idx = idx - defaults_start_idx in
      let d_expr = defaults.(d_idx) in
      acc := !acc ^ "=" ^ expr d_expr);
    (* check pos only *)
    if idx == pos_only_idx then acc := !acc ^ ", /"
  in
  Base.List.iteri all_args ~f:process_arg;
  !acc

and statement = function
  | Import _ -> ""
  | _ -> noop

and expr = function
  | BinOp { left; right; op; _ } -> expr left ^ " " ^ bin_op op ^ " " ^ expr right
  | Constant { value; _ } -> constant value
  | Lambda { args; body; _ } -> "lambda" ^ " " ^ arguments args ^ ": " ^ expr body
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
