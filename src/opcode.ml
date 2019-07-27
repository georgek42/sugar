type opcode =
  | Pushi of int
  | Pushr of int
  | Pop of int

  | Call of string
  | Ret

  | Addi
  | Divi
  [@@deriving show]