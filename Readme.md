Validation PPX Readme
=====================

The Validation PPX is designed to make it easy to statistically analyze datasets. Given a record type, `t`, you can annotate `t`s fields, and then call a single function to generate a report that describes the proportion of field values that are null, the number of field string values that are invalid, etc.

Example
-------

For example, given the record type, `t`, the following annotations tell the Validation library that that `t`s `a` field is a string that should equal "true" or "false"; and that its `b` field is an int that should be greater than 0.

```ocaml
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
```

Given a list of `t` values, `xs`, we can generate a validation report for `xs` by simply calling `Validations.report xs`. For example:

```ocaml
let xs = [ { a = Some "true"; b = 1 }; { a = Some "false"; b = 0 } ]

let () =
  Validations.report xs
  |> [%sexp_of: Ppx_validation_runtime.Report.t list]
  |> sprintf !"%{Sexp.to_string_hum}"
  |> print_endline
```

The resulting report will list information about the values of the fields in `xs`:

```lisp
(((field_name a_stats)
  (reports
   ((Str_enum
     ((num_values 2) (distrib ((false 1) (true 1))) (num_invalid_values 0)
      (prop_invalid_values 0))))))
 ((field_name b_stats)
  (reports
   ((Int_ranged
     ((num_values 2) (num_invalid_values 0) (prop_invalid_values 0) (min (0))
      (max (1))))))))
```

Usage in Other Projects
-----------------------

To use this PPX library in another project include the `ppx_validation_runtime` library in your Dune file and the `ppx_validation` PPX in your preprocess section. For example:

```
(executable
 (name main)
 (libraries
  core_kernel
  ppx_validation_runtime
  )
 (preprocess
  (pps ppx_jane ppx_validation))
  (modes exe)
)
```

Usage
-----

This library defines a variety of field types. These are listed below:

| Sort        | Field type | Spec type |
| ----------- | ---------- | --------- |
| Date_ranged | Date.t     | { start_date: Date.t option; end_date: Date.t option; granularity = Weeks|Months|Years } |
| Formatted   | string     | { format: Re.re } |
| Int_ranged  | int        | { low: int option; high: int option } |
| Linked      | 'a         | { get_num_matches : 'a -> int } |
| Nullable    | 'a option  | unit |
| Str_enum    | string     | { valid_values: String.Set.t } |

Each field annotation includes a record with three fields: `{ sort: sort; spec; transform }` where `sort` is one of the values listed above and `spec` depends on the sort.

For example:

```ocaml
[@validation
  {
    sort = Str_enum;
    spec = { valid_values = String.Set.of_list [ "true"; "false" ] };
    transform = Fn.id;
  }]
```

is a valid field annotation.

You must attach the PPX annotation to each type definition you are trying to analyze.

WARNING: this PPX depends on the fields PPX. You must annotate each type with both the `fields` and the `validation` PPXs.

This library will create a module named Validations in the same context as the type definition. To prevent this module from conflicting with other definitions, you may need to enclose the type definition in its own module.

To generate a report, call `Validations.report xs` where `xs` denotes the dataset and has type `xs : t list`.