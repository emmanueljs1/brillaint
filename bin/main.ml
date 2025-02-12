open Lib.Cfg
open Lib.Common
open Lib.Df
open Lib.Syntax
open Lib.Util

module Defn : Domain = struct
  type domain = StrSet.t
  
  let domain_eq = StrSet.equal
  
  let init = StrSet.empty
  
  let merge outs =
    IntMap.fold (fun _ -> StrSet.union) outs StrSet.empty
  
  let transfer d i =
    match dest_of_ins i with
    | Some (dst, _) -> StrSet.add dst d
    | _ -> d

  let string_of_domain d =
    if StrSet.is_empty d then
      "∅"
    else
      StrSet.to_list d |> String.concat ", "
end

module Defined = Analysis(Defn)

module Constants : Domain = struct
  type domain = (lit option) StrMap.t

  let domain_eq = StrMap.equal (=)

  let init = StrMap.empty

  let merge outs =
    IntMap.fold (fun _ ->
      StrMap.merge (fun _ v1 v2 ->
        match (v1, v2) with
        | v1, None -> v1
        | None, v2 -> v2
        | Some v1, Some v2 -> if v1 = v2 then Some v1 else Some None
      )
    ) outs StrMap.empty

  let transfer d i =
    match i with
    | Const ((dst, _), lit) ->
        StrMap.add dst (Some lit) d
    | Assign ((dst, _), _) ->
        StrMap.add dst None d
    | _ -> d

  let string_of_domain d =
    if StrMap.is_empty d then
      "∅"
    else
      StrMap.to_list d
      |> List.map (fun (x, v_opt) ->
          let v =
            match v_opt with
            | None -> "?"
            | Some lit -> json_of_lit lit |> Yojson.Raw.to_string
          in
          Printf.sprintf "%s -> %s" x v
      )
      |> String.concat ", "
end

module CProp = Analysis(Constants)

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
    match (try Sys.argv.(1) with Invalid_argument _ -> "const") with
    | "const" -> mk_analysis CProp.df Constants.string_of_domain
    | _ -> mk_analysis Defined.df Defn.string_of_domain
  in

  let p = Yojson.Raw.from_channel stdin |> prog_of_json in
  let cfgs = List.map (fun f -> (f.name, mk_cfg f.instrs)) p in
  List.iter (fun (fn, cfg) ->
    Printf.sprintf "%s:" fn |> print_endline; analysis.analyze cfg
  ) cfgs
