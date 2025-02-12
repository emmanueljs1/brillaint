open Lib.Cfg
open Lib.Common
open Lib.Df
open Lib.Syntax
open Lib.Util

let string_of_dom (d: domain) : string =
  StrSet.to_list d
  |> String.concat ", "
  |> Printf.sprintf "{ %s }"

let () =
  let p = Yojson.Raw.from_channel stdin |> prog_of_json in
  let cfgs = List.map (fun f -> (f.name, mk_cfg f.instrs)) p in
  List.iter (fun (fn, cfg) ->
    Printf.sprintf "%s:" fn |> print_endline;
    let (ins, outs) = df cfg in
    IntMap.iter (fun b _ ->
      let lbl =
        match IntMap.find_opt b cfg.lbls with
        | Some lbl -> lbl
        | None -> string_of_int b
      in
      Printf.sprintf "  %s:" lbl |> print_endline;
      let inb = IntMap.find b ins in
      string_of_dom inb |> Printf.sprintf "    in: %s" |> print_endline;
      let outb = IntMap.find b outs in
      string_of_dom outb |> Printf.sprintf "    out: %s" |> print_endline
    ) cfg.blocks
  ) cfgs
