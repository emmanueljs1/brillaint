open Cfg
open Common
open Syntax

module type Domain = sig
  type domain

  val domain_eq : domain -> domain -> bool

  val init : domain

  val merge : domain IntMap.t -> domain

  val transfer : domain -> ins -> domain

  val string_of_domain : domain -> string
end

type 'a dataflow = 'a IntMap.t * 'a IntMap.t

module Analysis (D : Domain) = struct
  open D
  
  let transfer_block (b: block) (inb: domain) : domain =
    List.fold_left transfer inb b
  
  let df (cfg: cfg) : domain dataflow =
    let rec loop ins outs bs =
      match IntSet.choose_opt bs with
      | None -> ins, outs
      | Some b ->
        let outb = IntMap.find b outs in
        let preds = predecessors b cfg in
        let inb =
          if b = cfg.entry then
            init
          else
            IntMap.filter (fun b' _ -> IntMap.mem b' preds) outs |> merge
        in
        let outb' = transfer_block (IntMap.find b cfg.blocks) inb in
        let bs' = IntSet.remove b bs in
        let bs'' =
          if domain_eq outb outb' then
            bs'
          else
            IntSet.fold IntSet.add bs' (IntMap.find b cfg.successors)
        in
        loop (IntMap.add b inb ins) (IntMap.add b outb' outs) bs''
    in
  
    let ins = IntMap.empty in
    let outs = IntMap.map (fun _ -> init) cfg.blocks in
    let worklist = IntMap.fold (fun b _ -> IntSet.add b) cfg.blocks IntSet.empty in
    loop ins outs worklist
end
