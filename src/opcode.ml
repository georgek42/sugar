type opcode =
  | Pushi of int
  | Pushr of int
  | Pop of int

  | Call of int
  | Syscall of int
  | Ret

  | Hdl

  | Addi
  | Divi
  [@@deriving show]

type program = opcode array

let show_program (p: program): string =
  let open Core in
  p |> Array.map ~f:show_opcode |> Array.to_list |> String.concat ~sep:"\n"