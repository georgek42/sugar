type opcode =
  | Pushi of int
  | Pushr of int
  | Pop of int

  | Call of int
  | Ret

  | Addi
  | Divi
  [@@deriving show]