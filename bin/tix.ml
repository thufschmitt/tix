let typecheck_chan chan =
  let ast = match Parse.(Parser.onix Lexer.read (Lexing.from_channel chan)) with
    | Some s -> Simple.Of_onix.expr s
    | None -> assert false
  in
  let t =
    Typing.(Typecheck.Infer.expr
              Types.Environment.default
              Typing_env.empty)
      ast
  in
  Typing.Types.pp Format.std_formatter t;
  Format.print_newline ()

let typecheck_file = function
  | "-" -> typecheck_chan stdin
  | fname -> CCIO.with_in fname typecheck_chan

open Cmdliner

let file_in =
  let doc = "Input file" in
  Arg.(value & pos 0 string "-" & info [] ~docv:"FILE" ~doc)

let eval_stuff = Term.(const typecheck_file $ file_in)
let info =
  let doc = "The nix type-checker" in
  let man = [] in
  Term.info "tix" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (eval_stuff, info)
