# Sugar -- a VM for Carml

A simple, slow, stack-based bytecode VM

# Interface
The VM executes bytecode defined in `src/opcode.ml`. A higher-level assembly-like language is available, which gets compiled to bytecode (`src/asm.ml`)

## Syscalls
* id: `1`, name: `print_int` -- prints an integer from the top of the stack (non-destructive_)

## Assembly
The assembly provides a higher-level interface to the VM, with labels instead of offsets. Forward-declarations are supported.
* `<label:>` -- define label at current offset
### Stack
* `pushi <int>` -- push int onto stack
* `pushr $<register_id` -- push from register onto stack
* `pop $<register_id>` -- pop from stack into register

### Control flow
* `call <offset>` -- call a function at offset, arguments passed in the stack in callee order
* `syscall <name>` -- execute a system call by name (see Syscalls)
* `ret` -- return from a function, return value on the top of the stack

### Binary operations
* `addi` -- add two integers from the stack
* `divi` -- divide the first integer by the second integer on the stack


## Bytecode
### Stack
* `pushi <int>` -- push int onto stack
* `pushr <register_id` -- push from register onto stack
* `pop <register_id>` -- pop from stack into register

### Control flow
* `call <offset>` -- call a function at offset, arguments passed in the stack in callee order
* `syscall <id>` -- execute a system call with id (see #syscalls)
* `ret` -- return from a function, return value on the top of the stack

### Binary operations
* `addi` -- add two integers from the stack
* `divi` -- divide the first integer by the second integer on the stack