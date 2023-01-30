open Cmdliner
open PyreAst

let read_file_content = function
  | "-" -> Stdio.In_channel.(input_all stdin) |> Result.ok
  | filename ->
    (try Stdio.In_channel.(read_all filename) |> Result.ok with
     | Sys_error message -> Result.error message)
;;

let parse ~f content =
  Parser.with_context (fun context ->
    f ~context content
    |> Result.map_error
         (fun { Parser.Error.message; line; column; end_line; end_column } ->
         Format.sprintf
           "Parse error at line %d, column %d to line %d, column %d: %s"
           line
           column
           end_line
           end_column
           message))
;;

let handle_result ~f = function
  | Result.Ok result -> f result
  | Result.Error message -> Format.eprintf "%s\n" message
;;

let parse_module enable_type_comment filename =
  let open Opine.Unparse.State in
  let result =
    let open Base.Result in
    read_file_content filename
    >>= fun content ->
    parse content ~f:(fun ~context content ->
      Parser.Concrete.parse_module ~context ~enable_type_comment content)
  in
  handle_result result ~f:(fun ast ->
    Format.printf "%a\n" Sexplib.Sexp.pp_hum (Concrete.Module.sexp_of_t ast);
    Format.printf "%s\n" (Opine.Unparse.py_module Opine.Unparse.State.default ast).source)
;;

let parse_expression filename =
  let result =
    let open Base.Result in
    read_file_content filename
    >>= fun content ->
    parse content ~f:(fun ~context content ->
      Parser.Concrete.parse_expression ~context content)
  in
  handle_result result ~f:(fun ast ->
    Format.printf "%a\n" Sexplib.Sexp.pp_hum (Concrete.Expression.sexp_of_t ast);
    Format.printf "%s\n" (Opine.Unparse.expr Opine.Unparse.State.default ast).source)
;;

let help_sections =
  [ `S Manpage.s_common_options
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; `Noblank
  ; `S Manpage.s_bugs
  ; `P "Check bug reports at https://github.com/grievejia/pyre-ast/issues."
  ]
;;

let filename_arg =
  let doc = "File name to be parsed. If the name is '-', read from stdin." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE_NAME" ~doc)
;;

let type_comment_arg =
  let doc = "Whether to parse type comments or not" in
  Arg.(value & flag & info [ "c"; "enable-type-comment" ] ~doc)
;;

let parse_expression_cmd =
  let doc = "Parse the given input file as a Python expression." in
  let man =
    [ `S Manpage.s_description
    ; `P
        "Read the given input file and parse it as a Python expression. Print the AST as \
         an s-expression to stdout"
    ; `Blocks help_sections
    ]
  in
  let info = Cmd.info "expression" ~doc ~man in
  Cmd.v info Term.(const parse_expression $ filename_arg)
;;

let parse_module_cmd =
  let doc = "Parse the given input file as a Python module." in
  let man =
    [ `S Manpage.s_description
    ; `P
        "Read the given input file and parse it as a Python module. Print the AST as an \
         s-expression to stdout"
    ; `Blocks help_sections
    ]
  in
  let info = Cmd.info "module" ~doc ~man in
  Cmd.v info Term.(const parse_module $ type_comment_arg $ filename_arg)
;;

let main_cmd =
  let doc = "A Python parser based on `pyre-ast` for idiom-ml" in
  let sdocs = Manpage.s_common_options in
  let info = Cmd.info "opine-parse" ~version:"dev" ~doc ~sdocs ~man:help_sections in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [ parse_expression_cmd; parse_module_cmd ]
;;

let () = Stdlib.exit (Cmd.eval main_cmd)
