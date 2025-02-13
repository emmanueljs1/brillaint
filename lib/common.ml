open Syntax

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)
module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let dest_of_ins : ins -> dest option = function
  | Const (dest, _) -> Some dest
  | Assign (dest, _) -> Some dest
  | _ -> None

let args_of_exp : exp -> id list = function
  | Binop (_, id1, id2) -> [id1; id2]
  | Unop (_, id) -> [id]
  | Call (_, ids) -> ids

let args_of_eff : eff -> id list = function
  | Br (id, _, _) -> [id]
  | Call (_, ids) -> ids
  | Ret id_opt ->
      begin match id_opt with
      | Some id -> [id]
      | None -> []
      end
  | Print id -> [id]
  | _ -> []

let args_of_ins (i: ins) : id list =
  match i with
  | Assign (_, e) -> args_of_exp e
  | Effect e -> args_of_eff e
  | _ -> []
