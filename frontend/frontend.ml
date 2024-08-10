open Ppxlib
open Parsetree

let is_rty attribute = String.equal "rty" attribute.attr_name.txt

let impl struc =
  let rtys, struc =
    List.partition
      (fun item ->
        match item.pstr_desc with
        (* NOTE: omit rec_flag; refinement type cannot be recursive *)
        | Pstr_value (_, [ value_binding ]) ->
            List.exists is_rty value_binding.pvb_attributes
        (* NOTE: omit mutural recursion*)
        | Pstr_value (_, _) -> false
        | _ -> false)
      struc
  in
  let () = Printf.printf "rtys:\n%s\n" (Pprintast.string_of_structure rtys) in
  struc

let intf intf = intf
let () = Driver.register_transformation ~impl ~intf "refinement type"
