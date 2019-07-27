open Core
open Sugar
open Sugar.Opcode

let () =
  let prog = [|
    (* main *)
    Pushi 3;
    Pushi 2;
    Pushi 1;
    Call "mean";
    Call "vm_print_int";
    Ret;
    (* mean (pc: 6) *)
    Pop 1;
    Addi;
    Addi;
    Pop 2;
    Pushi 3;
    Pushr 2;
    Divi;
    Pushr 1;
    Ret;
  |] in
  Vm.addl "mean" 6;
  Vm.run prog ~debug:false;