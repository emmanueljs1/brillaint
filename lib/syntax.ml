type id = string

type ty =
  | Int
  | Bool
  | Param of (id * ty)

type lit =
  | Int of int64
  | Bool of bool

type lbl = string
type dest = id * ty

type binop =
  | Add
  | Mul
  | Sub
  | Div
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or

type unop =
  | Not
  | Id

type exp =
  | Binop of (binop * id * id)
  | Unop of (unop * id)
  | Call of (id * id list)

type eff =
  | Jmp of lbl
  | Br of (id * lbl * lbl)
  | Call of (id * id list)
  | Ret of id option
  | Print of id
  | Nop

type ins =
  | Const of (dest * lit)
  | Assign of (dest * exp)
  | Effect of eff
  | Label of lbl

type arg = id * ty

type fn =
  { name: id
  ; args: arg list
  ; ret_ty: ty option
  ; instrs: ins list
  }

type prog = fn list
