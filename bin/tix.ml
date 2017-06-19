let parse_chan _fname chan =
  match MParser.parse_channel Parse.Parser.expr chan () with
  | MParser.Success t -> t
  | MParser.Failed (msg, _) ->
    Format.print_string msg;
    Format.print_flush ();
    exit 1

let typecheck ast =
  let typ =
    Simple.Of_onix.expr ast
    |> Typing.(Typecheck.Infer.expr
                 Environment.default)
  in
  CCResult.map
    (Format.fprintf Format.std_formatter "%a\n" Typing.Types.pp)
    typ

let process_file is_parse_only f_name =
  let ast =
    match f_name with
    | "-" -> parse_chan "-" stdin
    | fname -> CCIO.with_in fname (parse_chan fname)
  in
  if is_parse_only then
    Parse.Pp.pp_expr Format.std_formatter ast
  else
    ignore @@ CCResult.map_err
      (fun (loc, msg) ->
         Format.eprintf "error: %s, at %a\n"
           msg
           Parse.Location.pp loc;
         Format.pp_print_flush Format.err_formatter ();
         exit 1)
      (typecheck ast)

open Cmdliner

let file_in =
  let doc = "Input file" in
  Arg.(value & pos 0 string "-" & info [] ~docv:"FILE" ~doc)

let parse_only =
  let doc = "Do not typecheck, just parse the file" in
  Arg.(value & opt bool false & info [ "p"; "parse-only" ]
         ~docv:"PARSE_ONLY" ~doc)

let eval_stuff = Term.(const process_file $ parse_only $ file_in)
let info =
  let doc = "The nix type-checker" in
  let man = [] in
  Term.info "tix" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (eval_stuff, info)
