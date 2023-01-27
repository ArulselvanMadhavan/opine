open PyreAst
open Concrete.Expression
open Concrete.Constant
open Concrete.BinaryOperator

let noop = "noop"

let bin_op = function
  | Add -> "+"
  | Mult -> "*"
  | _ -> noop
;;

let constant = function
  | Integer i -> Int.to_string i
  | _ -> noop
;;

let arg a =
  let open Concrete.Argument in
  Concrete.Identifier.to_string a.identifier
;;

let rec arguments xs =
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

and expr = function
  | BinOp { left; right; op; _ } -> expr left ^ " " ^ bin_op op ^ " " ^ expr right
  | Constant { value; _ } -> constant value
  | Lambda { args; body; _ } -> "lambda" ^ " " ^ arguments args ^ ": " ^ expr body
  | Name { id; _ } -> Concrete.Identifier.to_string id
  | _ -> noop
;;
