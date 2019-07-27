# Sugar -- a VM for Carml

A simple, slow, stack-based bytecode VM

Example
```ml
let mean a b c = (a + b + c) / 3

let main () =
    let avg = mean 1 2 3 in
    print_int avg

```
```mips
mean:
    pop $1
    addi
    addi
    pop $2
    pushi 3
    pushr $2
    divi
    push $1
    ret

main:
    pushi 3
    pushi 2
    pushi 1
    call mean
    call vm_print_int
    ret
```

# Interface

# Operations

* Unlimited number of registers

## Stack
* `pushi <int>` -- push int onto stack
* `pushr <register_id` -- push from register onto stack
* `pop <register_id>` -- pop from stack into register

## Control flow
* `call <label>` -- call a function, arguments passed in the stack in callee order
* `ret` -- return from a function, return value on the top of the stack

## Binary operations
* `addi` -- add two integers from the stack
* `divi` -- divide the first integer by the second integer on the stack