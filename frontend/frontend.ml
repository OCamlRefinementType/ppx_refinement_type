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
  let () =
    List.iter
      (fun (name, rty) ->
        Printf.printf "%s : %s\n" (string_of_pattern name) (layout_rty rty))
      rtys_ctx
  in
  struc

let intf intf = intf
let () = Driver.register_transformation ~impl ~intf "refinement type"
