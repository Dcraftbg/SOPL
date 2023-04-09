# SOPL
Stack-oriented Programming Language

## About

> What is sopl?

Sopl is a lightweight, easily extendable, stack-oriented language.
It is currently in it's very early stages and is syntacically bound to change over time. I'll try to not change it semantically too much but be warned if you are using it to build projects yourself as things may break over versions.
For more info on current versions checkout `version.txt`
## Plans
> Source
- [x] externals
- [x] strings
- [x] register access
- [x] includes
- [x] functions
- [ ] control flow
- [ ] type checking
- [ ] structs
- [ ] struct methods
- [ ] interfaces


> Build platforms: 

Because of how the project is structured, it can easily be expanded to not only NASM assembly but to virtually any stack machine out there.
Current supports include:
- [x] NASM x86 64
- [ ] Java bytecode
- [ ] Native simulation

Ideas that are left for discusion and further expansion:
- [ ] ARM assembly
- [ ] C

## Requirements
**NASM**:
- Any nasm version that can support 64 bit assembly.
- Any linker or executable builder (gcc, ld, etc.)
## Startup
```cmd
cargo run (PLATFORM) (File path) -o (Output path)
```
## How to use?

*Note that documentation may not cover all of the latest features tho you might expect updates on them shortly after implementation*
### Hello World!
```sopl
extern "C" printf

func main(int, ptr : int) {
    "Hello World!\n" pop printf
}
```
### extern

**Syntax**:
```
extern [TYPE] (name)
```
Extern is a way to access outside features of the language or link to functions from other languages such as IO implementations from C etc.
It has to be noted here that using extern on its own can sometimes lead to unexpected behaviour or program slowdowns as most externs don't get type checked automatically and
might not be available for the current mode you are running in like if you try to link to standard C functions when you are in JAVA mode.

Thats also a reason why externs are generally not recommended for direct use (although depending on your version it might be required to use them raw (currently its required)).
### functions

**Syntax**:
```
func (name) ((contract)) {

}
```
Functions, just like in other languages have a name and a body. In sopl however you have to provide a contract:

A contract is a way to tell the type checker what to expect to have when it calls your function and in what state the top of the stack is going to be afterwards.
The contract contains:
(Input parameters separated by , : Output parameters separated by ,)

Inside of functions you can use the "ret" keyword to return although it is automatically done for you at the end of the functions declaration.

**Example(s)**:
```sopl
func sayHello() {
   "Hello World!\n" pop printf pop
}
func counter(long : long) {
    RBX pop
    RCX = 1
    RBX RCX -
    RBX push
}
```

### including

**Syntax**
```spl
include "Path/To/File"
```
Functions.spl:
```spl
extern "C" printf
func sayHello() {
   "Hello World!\n" pop printf pop
}
```
HelloWorld.spl:
```spl
include "./Functions.spl"
func main(int, ptr : int) {
    sayHello
}
```
#### registers
Current register support:
- [x] RAX
  - [x] EAX
    - [x] AX
      - [x] AH
      - [x] AL
- [x] RBX
  - [x] EBX
    - [x] BX
      - [x] BH
      - [x] BL
- [x] RCX
  - [x] ECX
    - [x] CX
      - [x] CH
      - [x] CL
- [x] RDX
  - [x] EDX
    - [x] DX
      - [x] DH
      - [x] DL
- [x] RSP
  - [x] ESP
- [x] RBP
  - [x] EBP

**[NOTE] RAX is used for returning longs/int/shorts/chars for C functions and your functions so usage of it outside of that is not advised** 
**Syntax**
```
(RegisterName) (Op | RegisterName) [Op]
```
**Example(s)**
```sopl
RBX pop   // Pops RBX off the stack
RCX = 4   // sets RCX
RBX RCX + // Adds RCX to RBX
```

Currently supported register operations:
```
(REG1) (REG2) + -> adds the two registers together (RESULT IN REG1)
(REG1) (REG2) - -> subtracts the second register from the first register (RESULT IN REG1)
(REG1) (REG2) * -> multiples the two registers     (RESULT IN REG1)
(REG1) push     -> pushes register onto the stack                          (WARNING: currently does not support 32 bit registers)
(REG1) pop      -> pops the value off the stack and loads it into register (WARNING: currently does not support 32 bit registers)
pop             -> pops 64 bit value off the stack and loads it into RAX
push            -> pushes RAX onto the stack
```
#### managing C functions and their return values

C functions are a little bit different than SOPL ones, As you know C actually copies its parameters when they are passed to a function and DOES NOT CONSUME them off the stack.
It also returns its integers, longs, shorts or chars (this also includes pointers) in the RAX register. 
Parameters to C functions are also passed backwards. As an example here is some C code:
```c
printf("%ld", 5);
```
And here is the same code in sopl:
```sopl
5l "%ld" pop printf
```
As you can see the parameters get passed in a reversed order (thats because of how your parameters in C get passed to functions normally)

As for returning values that are higher than 8 bytes, C pushes them on the stack together with everything else. 



