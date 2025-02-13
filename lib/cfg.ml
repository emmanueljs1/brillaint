open Common
open Syntax

type block = ins list

type blocks = block IntMap.t

type cfg =
  { entry: int
  ; exit: int
  ; successors: IntSet.t IntMap.t
  ; blocks: blocks
  ; lbls: string IntMap.t
  }

let is_terminator : eff -> bool = function
  | Ret _ | Jmp _ | Br _ -> true
  | _ -> false

let mk_blocks : ins list -> blocks * int =
  let rec loop (b: block) (k: int) (bs: blocks) : ins list -> blocks * int = function
    | Label s :: instrs ->
        begin match b with
        | [] -> loop [Label s] k bs instrs
        | _ -> loop [Label s] (k + 1) (IntMap.add k (List.rev b) bs) instrs
        end
    | Effect eff :: instrs when is_terminator eff ->
        loop [] (k + 1) (IntMap.add k (List.rev (Effect eff :: b)) bs) instrs
    | instr :: instrs -> loop (instr :: b) k bs instrs
    | [] ->
        begin match b with
        | [] ->
            if IntMap.is_empty bs then
              (IntMap.singleton 0 [], 1)
            else
              (bs, k)
        | _ -> (IntMap.add k (List.rev b) bs, k + 1)
        end
  in
  loop [] 0 IntMap.empty

let mk_successors (bs: blocks) (size: int) : IntSet.t IntMap.t =
  let lbls =
    IntMap.fold (fun k b acc ->
      match b with
      | Label s :: _ -> StrMap.add s k acc
      | _ -> acc
    ) bs StrMap.empty
  in
  IntMap.mapi (fun k b ->
    match List.rev b with
    | Effect (Jmp lbl) :: _ ->
        StrMap.find lbl lbls |> IntSet.singleton
    | Effect (Br (_, lbl1, lbl2)) :: _ ->
        IntSet.of_list [StrMap.find lbl1 lbls; StrMap.find lbl2 lbls]
    | Effect (Ret _) :: _ -> IntSet.empty
    | _ -> if k + 1 = size then IntSet.empty else IntSet.singleton (k + 1)
  ) bs

let mk_cfg (instrs: ins list) : cfg =
  let (bs, size) = mk_blocks instrs in
  let successors = mk_successors bs size in

  let lbls =
    IntMap.fold (fun k b acc ->
      match b with
      | Label s :: _ -> IntMap.add k s acc
      | _ -> acc
    ) bs IntMap.empty
  in

  let exits =
    IntMap.filter (fun k _ ->
      IntMap.find k successors |> IntSet.is_empty
    ) bs |> IntMap.to_list
  in

  match exits with
  | [(k, _)] ->
      { entry = 0; exit = k; successors = successors; blocks = bs; lbls = lbls }
  | _ ->
      { entry = 0
      ; exit = size
      ; successors =
        IntMap.map (fun succs ->
          if IntSet.is_empty succs then
            IntSet.singleton size
          else
            succs
        ) successors
        |> IntMap.add size IntSet.empty
      ; blocks = IntMap.add size [] bs
      ; lbls = lbls
      }

let predecessors (idx: int) (cfg: cfg) : IntSet.t =
  IntMap.filter (fun idx' _ ->
    IntSet.exists ((=) idx) (IntMap.find idx' cfg.successors)
  ) cfg.blocks
  |> fun bs -> IntMap.fold (fun i _ -> IntSet.add i) bs IntSet.empty
