open Syntax

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)
module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let dest_of_ins (i: ins) : dest option =
  match i with
  | Const (dest, _) -> Some dest
  | Assign (dest, _) -> Some dest
  | _ -> None
