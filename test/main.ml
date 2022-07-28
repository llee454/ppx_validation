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
[@@deriving fields] [@@deriving_inline validation]

let _ = (fun (_ : t) -> ())

module Validations = struct
  type stats = {
    a_stats: Ppx_validation_runtime.Str_enum.stats;
    b_stats: Ppx_validation_runtime.Int_ranged.stats;
  }
  [@@deriving fields]

  let stats_init () =
    {
      a_stats = Ppx_validation_runtime.Str_enum.stats_init ();
      b_stats = Ppx_validation_runtime.Int_ranged.stats_init ();
    }

  let _ = stats_init

  let get_stats xs =
    let open Ppx_validation_runtime.Report in
    List.fold xs ~init:(stats_init ()) ~f:(fun acc x ->
        Fields_of_stats.map
          ~a_stats:
            (app acc (fun s ->
                 a x
                 |> Fn.id
                 |> Option.value_map ~default:s ~f:(fun x ->
                        Ppx_validation_runtime.Str_enum.stats_update
                          { valid_values = String.Set.of_list [ "true"; "false" ] }
                          x s)))
          ~b_stats:
            (app acc (fun s ->
                 b x
                 |> Option.some
                 |> Option.value_map ~default:s ~f:(fun x ->
                        Ppx_validation_runtime.Int_ranged.stats_update { low = Some 0; high = None } x s))))

  let _ = get_stats

  let report_aux stats =
    let open Ppx_validation_runtime in
    Fields_of_stats.to_list
      ~a_stats:
        (Report.rep stats (fun s ->
             [ Str_enum (Str_enum.report { valid_values = String.Set.of_list [ "true"; "false" ] } s) ]))
      ~b_stats:
        (Report.rep stats (fun s -> [ Int_ranged (Int_ranged.report { low = Some 0; high = None } s) ]))

  let _ = report_aux

  let report xs = get_stats xs |> report_aux

  let _ = report
end

[@@@end]

let xs = [ { a = Some "true"; b = 1 }; { a = Some "false"; b = 0 } ]

let () =
  Validations.report xs
  |> [%sexp_of: Ppx_validation_runtime.Report.t list]
  |> sprintf !"%{Sexp.to_string_hum}"
  |> print_endline
