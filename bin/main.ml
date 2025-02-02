open Lib.Cfg
open Lib.Syntax
open Lib.Util

module IntMap = Map.Make(Int)

let () =
  let p = Yojson.Raw.from_file Sys.argv.(1) |> prog_of_json in
  let cfgs = List.map (fun f -> mk_cfg f.instrs) p in
  List.iter (fun cfg ->
    Printf.sprintf "entry: %d" cfg.entry |> print_endline;
    Printf.sprintf "exit: %d" cfg.exit |> print_endline;
    print_endline "blocks:";
    IntMap.iter (fun k b ->
      Printf.sprintf "block: %d" k |> print_endline;
      List.iter (fun i ->
        json_of_ins i |> Yojson.Raw.to_string |> print_endline
      ) b
    ) cfg.blocks;
    print_endline "edges:";
    IntMap.iter (fun k l ->
      List.iter (fun k' ->
        Printf.sprintf "%d -> %d" k k' |> print_endline
      ) l
    ) cfg.successors
  ) cfgs
