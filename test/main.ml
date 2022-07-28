open! Core_kernel

type t = {
  a: string option;
      [@validation
        {
          sort = Str_enum;
          spec = { valid_values = String.Set.of_list [ "true"; "false" ] };
          transform = Fn.id;
        }]
  b: int; [@validation { sort = Int_ranged; spec = { low = Some 0; high = None } }]
}
[@@deriving fields, validation]

let xs = [ { a = Some "true"; b = 1 }; { a = Some "false"; b = 0 } ]

let () =
  Validations.report xs
  |> [%sexp_of: Ppx_validation_runtime.Report.t list]
  |> sprintf !"%{Sexp.to_string_hum}"
  |> print_endline
