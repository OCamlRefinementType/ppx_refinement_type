open Ppxlib
open Parsetree

type rty =
  | RtyBase of { base_ty : core_type; phi : expression }
  | RtyArrow of { arg_name : pattern; arg_rty : rty; ret_rty : rty }

let string_of_pattern pattern =
  let _ = Format.flush_str_formatter () in
  Pprintast.pattern Format.str_formatter pattern;
  Format.flush_str_formatter ()

let rec layout_rty = function
  | RtyBase { base_ty; phi } ->
      Printf.sprintf "{v:%s | %s}"
        (string_of_core_type base_ty)
        (Pprintast.string_of_expression phi)
  | RtyArrow { arg_name; arg_rty; ret_rty } ->
      Printf.sprintf "%s:%s -> %s"
        (string_of_pattern arg_name)
        (layout_rty arg_rty) (layout_rty ret_rty)

let attr_is_rty attribute = String.equal "rty" attribute.attr_name.txt

let item_is_rty item =
  match item.pstr_desc with
  (* NOTE: omit rec_flag; refinement type cannot be recursive *)
  | Pstr_value (_, [ value_binding ]) ->
      List.exists attr_is_rty value_binding.pvb_attributes
  (* NOTE: omit mutural recursion*)
  | Pstr_value (_, _) -> false
  | _ -> false

let rec parse_rty expr =
  match expr.pexp_desc with
  | Pexp_let (_, bindings, ret_rty) ->
      List.fold_right
        (fun binding ret_rty ->
          RtyArrow
            {
              arg_name = binding.pvb_pat;
              arg_rty = parse_rty binding.pvb_expr;
              ret_rty;
            })
        bindings (parse_rty ret_rty)
  | Pexp_constraint (phi, base_ty) -> RtyBase { base_ty; phi }
  | _ -> failwith "die"

let parse_rty_binding value_binding =
  (value_binding.pvb_pat, parse_rty value_binding.pvb_expr)

let get_impl_from_typed_items name implementation =
  let open Ocaml_common.Typedtree in
  List.find_map
    (fun str ->
      match str.str_desc with
      | Tstr_value (_, [ value_binding ]) ->
          let pat =
            Ocaml_common.Untypeast.untype_pattern value_binding.vb_pat
          in
          if String.equal (string_of_pattern pat) (string_of_pattern name) then
            Some value_binding.vb_expr
          else None
      | _ -> None)
    implementation.structure.str_items

let impl struc =
  let rtys, struc = List.partition item_is_rty struc in
  let rtys_ctx =
    List.filter_map
      (fun item ->
        match item.pstr_desc with
        | Pstr_value (_, [ value_binding ]) ->
            Some (parse_rty_binding value_binding)
        | _ -> None)
      rtys
  in
  let implementation = Ocaml_typecheck.process_implementation_file struc in
  let () =
    List.iter
      (fun (name, rty) ->
        match get_impl_from_typed_items name implementation with
        | None ->
            Printf.printf "cannot find the implementation of function %s\n"
              (string_of_pattern name)
        | Some impl ->
            Printf.printf "Type judgement [%s]\n|-\n%s\n: %s\n"
              (string_of_pattern name)
              (Pprintast.string_of_expression
              @@ Ocaml_common.Untypeast.untype_expression impl)
              (layout_rty rty))
      rtys_ctx
  in
  struc

let intf intf = intf
let () = Driver.register_transformation ~impl ~intf "refinement type"
