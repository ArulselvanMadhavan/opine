open PyreAst.Concrete
open Base
(* Assumptions
   1. Module has only one class; everything is contained inside that class.
*)

module LinearCallInfo : sig
  type t =
    { idx : int
    ; param : string
    ; args : Expression.t list
    ; keywords : Keyword.t list
    }
  [@@deriving sexp_of, make]
end

module State : sig
  type t =
    { methods_count : int
    ; statement_count : int
    ; method_idx : (string, int) Hashtbl.t
    ; init_args : Arguments.t option
    ; linear_params : (string, LinearCallInfo.t list) Hashtbl.t
    ; class_name : string
    ; bmms : (string, int list) Hashtbl.t
    ; softmaxs : (string, int list) Hashtbl.t
    ; matmuls : (string, int list) Hashtbl.t
    ; members : string Hash_set.t
    }
  [@@deriving sexp_of, make]

  val default : t
end

val py_module : State.t -> Module.t -> State.t
