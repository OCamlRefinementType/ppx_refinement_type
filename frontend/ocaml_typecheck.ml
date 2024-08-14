open Ocaml_common

let initial_env () =
  let () = Compmisc.init_path () in
  let open_implicit_modules = [] in
  Typemod.initial_env
    ~loc:(Location.in_file "ocamldoc command line")
    ~open_implicit_modules ~initially_opened_module:(Some "Stdlib")
    ~safe_string:true

let process_implementation_file parsetree =
  let env = initial_env () in
  try
    let typedtree = Typemod.type_implementation "" "" "" env parsetree in
    typedtree
  with
  | Syntaxerr.Error _ as exn ->
      (match Location.error_of_exn exn with
      | Some (`Ok err) -> Location.print_report Format.err_formatter err
      | _ -> assert false);
      assert false
  | Failure s ->
      prerr_endline s;
      assert false
