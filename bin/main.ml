open Lib.Cfg
open Lib.Common
open Lib.Df
open Lib.Syntax
open Lib.Util

module Definitions : Domain = struct
  type domain = StrSet.t
  
  let domain_eq = StrSet.equal
  
  let init = StrSet.empty
  
  let merge = StrSet.union
  
  let transfer d i =
    match dest_of_ins i with
    | Some (dst, _) -> StrSet.add dst d
    | _ -> d

  let string_of_domain d =
    if StrSet.is_empty d then
      "∅"
    else
      StrSet.to_list d |> String.concat ", "

  let dir = Forward
end

module Defined = Analysis(Definitions)

module Constants : Domain = struct
  type value =
    | Constant of lit
    | Unknown

  type domain = value StrMap.t

  let domain_eq = StrMap.equal (=)

  let init = StrMap.empty

  let merge =
    StrMap.merge (fun _ v1 v2 ->
      match v1, v2 with
      | _, None -> v1
      | None, _ -> v2
      | Some v1, Some v2 -> if v1 = v2 then Some v1 else Some Unknown
    )

  let transfer d i =
    match i with
    | Const ((dst, _), lit) -> StrMap.add dst (Constant lit) d
    | Assign ((dst, _), _) -> StrMap.add dst Unknown d
    | _ -> d

  let string_of_value v =
    match v with
    | Unknown -> "?"
    | Constant lit -> json_of_lit lit |> Yojson.Raw.to_string

  let string_of_domain d =
    if StrMap.is_empty d then
      "∅"
    else
      StrMap.to_list d
      |> List.map (fun (x, v) ->
          string_of_value v |> Printf.sprintf "%s -> %s" x
      )
      |> String.concat ", "

  let dir = Forward
end

module CProp = Analysis(Constants)

module Variables : Domain = struct
  type domain = StrSet.t

  let domain_eq = StrSet.equal
  
  let init = StrSet.empty
  
  let merge = StrSet.union
  
  let transfer d i =
    let d' =
      match dest_of_ins i with
      | Some (dst, _) -> StrSet.remove dst d
      | _ -> d
    in
    args_of_ins i |> List.fold_left (fun d'' id -> StrSet.add id d'') d'

  let string_of_domain d =
    if StrSet.is_empty d then
      "∅"
    else
      StrSet.to_list d |> String.concat ", "

  let dir = Backward
end

module LiveVariables = Analysis(Variables)

type analysis = { analyze : cfg -> unit }

let mk_analysis (df: cfg -> 'a dataflow) (show: 'a -> string) : analysis =
  { analyze = fun cfg ->
    let (ins, outs) = df cfg in
    IntMap.iter (fun b _ ->
      let lbl =
        match IntMap.find_opt b cfg.lbls with
        | Some lbl -> lbl
        | None -> string_of_int b
      in
      Printf.sprintf "  %s:" lbl |> print_endline;
      let inb = IntMap.find b ins in
      show inb |> Printf.sprintf "    in: %s" |> print_endline;
      let outb = IntMap.find b outs in
      show outb |> Printf.sprintf "    out: %s" |> print_endline
    ) cfg.blocks
  }

let () =
  let analysis =
    match (try Sys.argv.(1) with Invalid_argument _ -> "...") with
    | "const" -> mk_analysis CProp.df Constants.string_of_domain
    | "defined" -> mk_analysis Defined.df Definitions.string_of_domain
    | "live" -> mk_analysis LiveVariables.df Variables.string_of_domain
    | _ -> failwith "invalid dataflow analysis"
  in

  let p = Yojson.Raw.from_channel stdin |> prog_of_json in
  let cfgs = List.map (fun f -> (f.name, mk_cfg f.instrs)) p in
  List.iter (fun (fn, cfg) ->
    Printf.sprintf "%s:" fn |> print_endline; analysis.analyze cfg
  ) cfgs
