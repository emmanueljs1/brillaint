open Cfg
open Common
open Syntax

type df_dir = Forward | Backward

module type Domain = sig
  type domain

  val domain_eq : domain -> domain -> bool

  val init : domain

  val merge : domain -> domain -> domain

  val transfer : domain -> ins -> domain

  val string_of_domain : domain -> string

  val dir : df_dir
end

type 'a dataflow = 'a IntMap.t * 'a IntMap.t

module Analysis (D : Domain) = struct
  open D
  
  let transfer_block (b: block) (inb: domain) : domain =
    match dir with
    | Forward -> List.fold_left transfer inb b
    | Backward -> List.fold_left transfer inb (List.rev b)

  let merge_all (ds: domain IntMap.t) : domain =
    IntMap.fold (fun _ -> merge) ds init

  let df (cfg: cfg) : domain dataflow =
    let get_prev b =
      match dir with
      | Forward -> predecessors b cfg
      | Backward -> IntMap.find b cfg.successors
    in

    let get_next b =
      match dir with
      | Forward -> IntMap.find b cfg.successors
      | Backward -> predecessors b cfg
    in

    let is_init b =
      match dir with
      | Forward -> b = cfg.entry
      | Backward -> b = cfg.exit
    in

    let rec loop ins outs bs =
      match IntSet.choose_opt bs with
      | None -> ins, outs
      | Some b ->
        let outb = IntMap.find b outs in

        let prev = get_prev b in

        let inb =
          if is_init b then
            init
          else
            IntMap.filter (fun b' _ -> IntSet.mem b' prev) outs |> merge_all
        in

        let outb' = transfer_block (IntMap.find b cfg.blocks) inb in

        let bs' = IntSet.remove b bs in
        let bs'' =
          if domain_eq outb outb' then
            bs'
          else
            IntSet.fold IntSet.add bs' (get_next b)
        in

        loop (IntMap.add b inb ins) (IntMap.add b outb' outs) bs''
    in
  
    let ins = IntMap.empty in
    let outs = IntMap.map (fun _ -> init) cfg.blocks in
    let worklist = IntMap.fold (fun b _ -> IntSet.add b) cfg.blocks IntSet.empty in

    match dir with
    | Forward -> loop ins outs worklist
    | Backward ->
        let outs', ins' = loop ins outs worklist in
        ins', outs'
end

(*
out[entry] = init
in[*] = init

worklist = all blocks
while worklist is not empty:
    b = pick any block from worklist
    out[b] = merge(in[p] for every successor p of b)
    in[b] = transfer(b, out[b])
    if in[b] changed:
        worklist += predecessors of b

*)
