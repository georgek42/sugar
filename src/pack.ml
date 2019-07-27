open Core
open Bitstring
open Opcode

let make_unary (op: int) (param: int): bitstring =
  let param = Int32.of_int_exn param in
  let%bitstring b = {|
    op : 8;
    param : 32
  |} in b

let make (op: int): bitstring =
  let%bitstring b = {|
    op : 8
  |} in b

exception Not_implemented

let pack (op: opcode): bitstring =
  match op with
  | Pushi i -> make_unary 0 i
  | Pushr rid -> make_unary 1 rid
  | Pop i -> make_unary 2 i
  | Syscall id -> make_unary 7 id
  | Call pc -> make_unary 6 pc
  | Ret -> make 3
  | Addi -> make 4
  | Divi -> make 5
  | Hdl -> make 8
  | Jc pc -> make_unary 9 pc
  | Pushnil -> make 10
  | Cons -> make 11
  | Jmp pc -> make_unary 12 pc
  | Halt -> make 13

let unpack (b: bitstring): program =
  let b = ref b in
  let res = Stack.create () in
  let shift_by n = b := subbitstring !b n (bitstring_length !b - n) in
  let add v = Stack.push res v in
  while Bitstring.bitstring_length !b >= 8 do
    match%bitstring !b with
    | {| op : 8 |} -> (
      match op with
      | 0 -> (
        match%bitstring !b with
        | {| op: 8; param: 32 |} -> Pushi (Int32.to_int_exn param) |> add; shift_by 40
        | {| _ |} -> raise Not_implemented
      )
      | 1 -> (
        match%bitstring !b with
        | {| op: 8; param: 32 |} -> Pushr (Int32.to_int_exn param) |> add; shift_by 40
        | {| _ |} -> raise Not_implemented
      )
      | 2 -> (
        match%bitstring !b with
        | {| op: 8; param: 32 |} -> Pop (Int32.to_int_exn param) |> add; shift_by 40
        | {| _ |} -> raise Not_implemented
      )
      | 6 -> (
        match%bitstring !b with
        | {| op: 8; param: 32 |} -> Call (Int32.to_int_exn param) |> add; shift_by 40
        | {| _ |} -> raise Not_implemented
      )
      | 7 -> (
        match%bitstring !b with
        | {| op: 8; param: 32 |} -> Syscall (Int32.to_int_exn param) |> add; shift_by 40
        | {| _ |} -> raise Not_implemented
      )
      | 9 -> (
        match%bitstring !b with
        | {| op: 8; param: 32 |} -> Jc (Int32.to_int_exn param) |> add; shift_by 40
        | {| _ |} -> raise Not_implemented
      )
      | 12 -> (
        match%bitstring !b with
        | {| op: 8; param: 32 |} -> Jmp (Int32.to_int_exn param) |> add; shift_by 40
        | {| _ |} -> raise Not_implemented
      )
      | 3 -> add Ret; shift_by 8
      | 4 -> add Addi; shift_by 8
      | 5 -> add Divi; shift_by 8
      | 8 -> add Hdl; shift_by 8
      | 10 -> add Pushnil; shift_by 8
      | 11 -> add Cons; shift_by 8
      | 13 -> add Halt; shift_by 8
      | _ -> raise Not_implemented
  )
  done;
  let res = Stack.to_array res in
  Array.rev_inplace res;
  res

let pack_all (prog: program): bitstring =
  prog |> Array.map ~f:pack |> Array.to_list |> Bitstring.concat

let to_file (filename: string) (prog: program) =
  Bitstring.bitstring_to_file (pack_all prog) filename

let from_file (filename: string) = Bitstring.bitstring_of_file filename |> unpack

let%test "pack_roundtrip" =
  let prog = [|
    Pushi 1;
    Pushr 6;
    Pop 3;
    Call 4;
    Syscall 0;
    Jc 12;
    Ret;
    Addi;
    Hdl;
    Halt;
    Cons;
    Jmp 11;
    Pushnil;
    Divi
  |] in
  let packed = pack_all prog in
  let unpacked = packed |> unpack in
  match Array.zip prog unpacked with
  | None -> false
  | Some l -> Array.fold l ~init:true ~f:(fun acc (op, op') -> acc && (op = op'))

let %test "pack_file_roundtrip" =
  let prog = [|
    Pushi 1;
    Pushr 6;
    Pop 3;
    Call 4;
    Syscall 0;
    Jc 12;
    Ret;
    Addi;
    Hdl;
    Halt;
    Cons;
    Jmp 11;
    Pushnil;
    Divi
  |] in
  prog |> to_file "tmp.sg";
  let unpacked = from_file "tmp.sg" in
  Sys.remove "tmp.sg";
  match Array.zip prog unpacked with
  | None -> false
  | Some l -> Array.fold l ~init:true ~f:(fun acc (op, op') -> acc && (op = op'))
