(**
  This module defines data types for generating data validation reports.
*)

open! Core_kernel

type values_report =
  | Nil
  | Str_enum    of Str_enum.report
  | Nullable    of Nullable.report
  | Formatted   of Formatted.report
  | Int_ranged  of Int_ranged.report
  | Date_ranged of Date_ranged.report
  | Linked      of Linked.report
[@@deriving sexp]

type t = {
  field_name: string;
  reports: values_report list;
}
[@@deriving sexp]

(**
  A convenience function that makes it easier to create field reports.

  Accepts three arguments:

  * stats, the dataset stats which contain the field stats as a
    record field
  * f, a function that takes a field spec and returns a field report
  * and field, a record field

  and returns a field report.

  Example:

  let example stats : t list =
    Fields_of_stats.to_list
      ~a:(rep stats (Fn.const [Nil_report]))
      ~b:(rep stats (Fn.const [Nil_report]))
*)
let rep stats f field = { field_name = Field.name field; reports = f (Field.get field stats) }

(**
  Another convenience function that makes it easier to compute fields stats.

  Accepts three arguments:

  * stats, the dataset stats which contain the field stats as a
    record field
  * f, a function that takes a field spec and returns a field report
  * and field, a record field

  and returns f applied to the field value.
*)
let app stats f field = Field.get field stats |> f
