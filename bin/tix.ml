let parse_chan fname chan =
  match MParser.parse_channel (Parse.Parser.expr fname) chan () with
  | MParser.Success t -> t
  | MParser.Failed (msg, _) ->
    Format.print_string msg;
    Format.print_flush ();
    exit 1

let typecheck ast =
  Simple.Of_onix.expr ast
  |> Typing.(Typecheck.Infer.expr
               Environment.default)

let process_file is_parse_only f_name =
  let ast =
    match f_name with
    | "-" -> parse_chan "-" stdin
    | _ -> CCIO.with_in f_name (parse_chan f_name)
  in
  if is_parse_only then
    Parse.Pp.pp_expr Format.std_formatter ast
  else
    let typed = typecheck ast in
    let log = Typing.Typecheck.W.log typed
    and value = Typing.Typecheck.W.value typed
    in
    CCList.iter (fun t ->
        Typing.Warning.pp Format.err_formatter t;
        Format.pp_print_newline Format.err_formatter ())
      (CCList.rev log);
    Format.fprintf Format.std_formatter "%a\n" Typing.Types.pp value

open Cmdliner

let file_in =
  let doc = "Input file" in
  Arg.(value & pos 0 string "-" & info [] ~docv:"FILE" ~doc)

let parse_only =
  let doc = "Do not typecheck, just parse the file" in
  Arg.(value & flag & info [ "p"; "parse-only" ] ~docv:"PARSE_ONLY" ~doc)

let eval_stuff = Term.(const process_file $ parse_only $ file_in)
let info =
  let doc = "The nix type-checker" in
  let man = [] in
  Term.info "tix" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (eval_stuff, info)
