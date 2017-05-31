let typecheck_chan fname chan =
  let ast = match MParser.parse_channel Parse.Parser.expr chan () with
    | MParser.Success t -> Simple.Of_onix.expr t
    | MParser.Failed (msg, _) ->
      Format.print_string msg;
      Format.print_flush ();
      exit 1
  in
  let t =
    Typing.(Typecheck.Infer.expr
              Types.Environment.default
              Typing_env.initial)
      ast
  in
  Typing.Types.pp Format.std_formatter t;
  Format.print_newline ()

let typecheck_file name =
  try
    begin
      match name with
      | "-" -> typecheck_chan "-" stdin
      | fname -> CCIO.with_in fname (typecheck_chan fname)
    end
  with
    Typing.Typecheck.TypeError (loc, msg) ->
    Format.eprintf "error: %s, at %a\n"
      msg
      Parse.Location.pp loc;
    Format.pp_print_flush Format.err_formatter ();
    exit 1

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
