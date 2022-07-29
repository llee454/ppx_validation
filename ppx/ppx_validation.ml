open Core_kernel
open Ppxlib
open Ast_builder.Default

type attrib = {
  sort_name: string;
  spec: expression;
  transform: expression option;
}

let sort_attribute =
  Attribute.declare "validation" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (let sort_name sort_expr =
       match sort_expr.pexp_desc with
       | Pexp_construct ({ txt = Lident sort_name; loc = _ }, _)
        |Pexp_ident { txt = Lident sort_name; loc = _ } ->
         sort_name
       | _ -> (* TODO fail properly *) failwith "BAD ID"
     in
     function
     | [%expr { sort = [%e? sort_expr]; spec = [%e? spec] }] ->
       { sort_name = sort_name sort_expr; spec; transform = None }
     | [%expr { sort = [%e? sort_expr]; spec = [%e? spec]; transform = [%e? transform] }] ->
       { sort_name = sort_name sort_expr; spec; transform = Some transform }
     | _ -> (* TODO fail properly *) failwith "NOT STRUCT")

let create_record ~loc pairs =
  pexp_record ~loc (List.map pairs ~f:(fun (name, exp) -> Located.lident ~loc name, exp)) None

type sort =
  | Nil
  | Str_enum
  | Nullable
  | Formatted
  | Int_ranged
  | Date_ranged
  | Linked
[@@deriving variants]

let sort_of_string = function
| "Nil" -> Some Nil
| "Str_enum" -> Some Str_enum
| "Nullable" -> Some Nullable
| "Formatted" -> Some Formatted
| "Int_ranged" -> Some Int_ranged
| "Date_ranged" -> Some Date_ranged
| "Linked" -> Some Linked
| _ -> None

let sort_to_stat_type ~loc = function
| Nil -> [%type: unit]
| Str_enum -> [%type: Ppx_validation_runtime.Str_enum.stats]
| Nullable -> [%type: Ppx_validation_runtime.Nullable.stats]
| Formatted -> [%type: Ppx_validation_runtime.Formatted.stats]
| Int_ranged -> [%type: Ppx_validation_runtime.Int_ranged.stats]
| Date_ranged -> [%type: Ppx_validation_runtime.Date_ranged.stats]
| Linked -> [%type: Ppx_validation_runtime.Linked.stats]

let sort_to_stat_init ~loc = function
| Nil -> [%expr ()]
| Str_enum -> [%expr Ppx_validation_runtime.Str_enum.stats_init ()]
| Nullable -> [%expr Ppx_validation_runtime.Nullable.stats_init ()]
| Formatted -> [%expr Ppx_validation_runtime.Formatted.stats_init ()]
| Int_ranged -> [%expr Ppx_validation_runtime.Int_ranged.stats_init ()]
| Date_ranged -> [%expr Ppx_validation_runtime.Date_ranged.stats_init ()]
| Linked -> [%expr Ppx_validation_runtime.Linked.stats_init ()]

let sort_to_stats_update ~loc ?(transform : expression = [%expr Option.some]) field_spec get_field =
  function
| Nil -> [%expr Fn.const ()]
| Str_enum ->
  [%expr
    fun s ->
      [%e get_field]
      |> [%e transform]
      |> Option.value_map ~default:s ~f:(fun x ->
             Ppx_validation_runtime.Str_enum.stats_update [%e field_spec] x s)]
| Nullable ->
  [%expr
    fun s ->
      [%e get_field]
      |> [%e transform]
      |> Option.value_map ~default:s ~f:(fun x ->
             Ppx_validation_runtime.Nullable.stats_update [%e field_spec] x s)]
| Formatted ->
  [%expr
    fun s ->
      [%e get_field]
      |> [%e transform]
      |> Option.value_map ~default:s ~f:(fun x ->
             Ppx_validation_runtime.Formatted.stats_update [%e field_spec] x s)]
| Int_ranged ->
  [%expr
    fun s ->
      [%e get_field]
      |> [%e transform]
      |> Option.value_map ~default:s ~f:(fun x ->
             Ppx_validation_runtime.Int_ranged.stats_update [%e field_spec] x s)]
| Date_ranged ->
  [%expr
    fun s ->
      [%e get_field]
      |> [%e transform]
      |> Option.value_map ~default:s ~f:(fun x ->
             Ppx_validation_runtime.Date_ranged.stats_update [%e field_spec] x s)]
| Linked ->
  [%expr
    fun s ->
      [%e get_field]
      |> [%e transform]
      |> Option.value_map ~default:s ~f:(fun x ->
             Ppx_validation_runtime.Linked.stats_update [%e field_spec] x s)]

let sort_to_report ~loc field_spec = function
| Nil -> [%expr rep stats (Fn.const [ Nil ])]
| Str_enum -> [%expr Report.rep stats (fun s -> [ Str_enum (Str_enum.report [%e field_spec] s) ])]
| Nullable -> [%expr Report.rep stats (fun s -> [ Nullable (Nullable.report [%e field_spec] s) ])]
| Formatted -> [%expr Report.rep stats (fun s -> [ Formatted (Formatted.report [%e field_spec] s) ])]
| Int_ranged -> [%expr Report.rep stats (fun s -> [ Int_ranged (Int_ranged.report [%e field_spec] s) ])]
| Date_ranged ->
  [%expr Report.rep stats (fun s -> [ Date_ranged (Date_ranged.report [%e field_spec] s) ])]
| Linked -> [%expr Report.rep stats (fun s -> [ Linked (Linked.report [%e field_spec] s) ])]

type spec = {
  field_name: string;
  sort: sort;
  spec: expression;
  transform: expression option;
}
[@@deriving fields]

let get_field_spec (ld : label_declaration) =
  let open Option in
  Attribute.get sort_attribute ld >>= fun { sort_name; spec; transform } ->
  sort_of_string sort_name >>| fun sort -> { field_name = ld.pld_name.txt; sort; spec; transform }

let get_stats_decl ~loc specs =
  let rec_fields =
    List.map specs ~f:(fun { field_name; sort; spec = _; transform = _ } ->
        label_declaration ~loc
          ~name:{ txt = sprintf "%s_stats" field_name; loc }
          ~mutable_:Immutable ~type_:(sort_to_stat_type ~loc sort))
  in
  let rec_decl : type_declaration =
    type_declaration ~loc ~name:{ txt = "stats"; loc } ~params:[] ~cstrs:[]
      ~kind:(Ptype_record rec_fields) ~private_:Public ~manifest:None
  in
  let annot_rec_decl =
    {
      rec_decl with
      ptype_attributes =
        [
          attribute ~loc ~name:{ txt = "deriving"; loc }
            ~payload:(PStr [ pstr_eval ~loc (pexp_ident ~loc { txt = Lident "fields"; loc }) [] ]);
        ];
    }
  in
  pstr_type ~loc Recursive [ annot_rec_decl ]

let get_stats_init_def ~loc specs =
  let rec_fields =
    List.map specs ~f:(fun { field_name; sort; _ } ->
        sprintf "%s_stats" field_name, sort_to_stat_init ~loc sort)
  in
  let record = create_record ~loc rec_fields in
  [%stri let stats_init () = [%e record]]

let get_stats_def ~loc specs =
  let args =
    List.map specs ~f:(fun { field_name; sort; spec; transform } ->
        let get_field =
          pexp_apply ~loc (pexp_ident ~loc { txt = Lident field_name; loc }) [ Nolabel, [%expr x] ]
        in
        let stats_update = sort_to_stats_update ~loc ?transform spec get_field sort in
        Labelled (sprintf "%s_stats" field_name), [%expr app acc [%e stats_update]])
  in
  let fn = pexp_apply ~loc [%expr Fields_of_stats.map] args in
  [%stri
    let get_stats xs =
      let open Ppx_validation_runtime.Report in
      List.fold xs ~init:(stats_init ()) ~f:(fun acc x -> [%e fn])]

let get_report_aux_def ~loc specs =
  let args =
    List.map specs ~f:(fun { field_name; sort; spec; transform = _ } ->
        Labelled (sprintf "%s_stats" field_name), sort_to_report ~loc spec sort)
  in
  let fn = pexp_apply ~loc [%expr Fields_of_stats.to_list] args in
  [%stri
    let report_aux stats =
      let open Ppx_validation_runtime in
      [%e fn]]

let get_report_def ~loc = [%stri let report xs = get_stats xs |> report_aux]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let items =
    List.map type_declarations ~f:(fun (td : type_declaration) ->
        match td with
        | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; ptype_loc; _ } ->
          let ext = Location.raise_errorf ~loc:ptype_loc "Cannot derive accessors for non record types" in
          [ Ast_builder.Default.pstr_extension ~loc ext [] ]
        | { ptype_kind = Ptype_record fields; _ } ->
          let specs = List.filter_map fields ~f:get_field_spec in
          [
            get_stats_decl ~loc specs;
            get_stats_init_def ~loc specs;
            get_stats_def ~loc specs;
            get_report_aux_def ~loc specs;
            get_report_def ~loc;
          ])
    |> List.concat
  in
  [
    {
      pstr_desc =
        Pstr_module
          (module_binding ~loc ~name:{ txt = Some "Validations"; loc }
             ~expr:{ pmod_desc = Pmod_structure items; pmod_loc = loc; pmod_attributes = [] });
      pstr_loc = loc;
    };
  ]

let generate_intf ~ctxt:_ (_rec_flag, _type_declarations) : signature_item list = []

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let ppx_validation = Deriving.add "validation" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator
