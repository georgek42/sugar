open Core
open Sugar
open Bitstring

let help () =
  printf "
Sugar 0.0.1
Commands:
  run <asm_file>.sugar -- Compile and execute a sugar assembly file
  compile <asm_file>.sugar <target_file>.sg -- Compile a sugar assembly file to bytecode
  exec <bytecode>.sg -- Execute a sugar bytecode file

Flags:
  --debug -- Enable debug messages
"

let () =
  if Array.length Sys.argv < 3 then help () else (
    match Array.get Sys.argv 1 with
    | "run" -> (
      let filename = Array.get Sys.argv 2 in
      let debug = if Sys.argv |> Array.filter ~f:(fun a -> a = "--debug") |> Array.length > 0 then true else false in
      let prog = Asm.from_file filename |> Compile.compile in
      Vm.run prog ~debug:debug
    )
    | "compile" when Array.length Sys.argv < 4 -> help ()
    | "compile" -> (
      let filename = Array.get Sys.argv 2 in
      let out_filename = Array.get Sys.argv 3 in
      Asm.from_file filename |> Compile.compile |> Pack.to_file out_filename
    )
    | "exec" -> (
      let filename = Array.get Sys.argv 2 in
      let debug = if Sys.argv |> Array.filter ~f:(fun a -> a = "--debug") |> Array.length > 0 then true else false in
      let prog = Pack.from_file filename in
      Vm.run prog ~debug:debug
    )
    | c -> printf "Unrecognized command: %s" c; help ();
  )