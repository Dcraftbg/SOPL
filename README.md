# SOPL
A C like Programming Language

With Rust like syntax with the same amount of low level programming as C

## About

> What is sopl?

Sopl is a lightweight, easily extendable, programming language.
It is currently in it's very early stages and is syntacically bound to change over time. I'll try to not change it semantically too much but be warned if you are using it to build projects yourself as things may break over versions.

For more info on current versions checkout [version.md](version.md)

For more info on what im currently working on checkout the recently started trello page: https://trello.com/b/OjR79UQt/sopl

## Manual
Made for easy navigation around the README.md
- [Quickstart](#quickstart)
- [Build platforms](#build)
- [Requirements](#Requirements)
- [Tools](#tools)
- [Flags and versions](#flags-and-versions)
- [NEWS](news.md)
- [Plans](#plans)
- [Highlighting](#highlighting)
- [How-to-use](#how-to-use)
  - [Hello-World](#hello-world)
  - [Locals](#locals)
  - [Constants](#constants)
  - [Expressions](#expressions)
  - [Including](#including)
  - [Functions](#functions)
  - [Control-flow](#control-flow)
    - [Ifs](#if-statements)
    - [While](#while-loops)
    - [makelabel](#makelabel)
    - [goto](#goto)
  - [Strings](#strings)
  - [Managing-c-functions](#managing-c-functions-and-their-return-values)
  - [Externals](#extern)
  - [Buffers](#buffers)
  - [Interrupts](#interrupts)
  - [Registers](#registers)
  - [Dlls](#dlls)
- [Libraries](#libs)


## Quickstart
```cmd
cargo run (File path) <-o (Output path)> <-t (PLATFORM)>
```
Another thing worth noting:
[NOTE] If you are experiencing an error of the following type:
```
[NOTE] No architecture found for {OS}_{Arch}! Please specify the output architecture!
```
It is probably because the compiler can't find a proper architecture for your current operating system and Arch. To fix this you can use the -arc flag with - and the path to the architecture configuration (examples [here](examples/arcs/), This should fix the error although it could cause issues if the configuration isn't correct) Like so:
```
sopl myprogram.spl -arc - myarcconfig.json 
```

## Build
> Build platforms: 

Because of how the project is structured, it can easily be expanded to not only NASM assembly but to virtually any assembler out there.
Current supports include:
- [x] NASM x86 64
- [ ] FASM x86 64
- [ ] Java bytecode
- [ ] Native simulation

Ideas that are left for discussion and further expansion:
- [ ] ARM assembly
- [ ] C

## Requirements
### NASM
- Any nasm version that can support 64 bit or 32 bit assembly.
- Any linker or builder (gcc, ld, etc.) - (note that whilst you can use libc, you can also use native features: i.e. syscalls on Linux (checkout the linux folder under libs) and Windows dll functions - not yet fully implemented)

## Tools
SOPL has a few tools to make your life easier!

One of the most major ones is [lighthouse](https://github.com/Dcraftbg/Lighthouse). Lighthouse makes organizing your projects A LOT simpler.
It also makes building, testing and running your code a lot easier.
You can follow the "How to get started" over at the repository (https://github.com/Dcraftbg/Lighthouse). 
It is highly recommended to use lighthouse! Lighthouse is an awesome tool that should make it better and easier to build with SOPL - you don't have to manage any flags or anything, everything is built for you, however this doesn't stop the endless customization you can have with normal SOPL - simply edit the lighthouse.cfg file that gets generated on every new project. For more information check out the repo.

One other tool that isn't as major is sopl_test which is generally only recommended to be used by the developer of the language since its only purpose is to test if all of the examples work as intentionally

## Flags and Versions

Whenever something gets added you will see that 99% of the time [version.md](version.md) gets updated. Thats because it contains the necessary information about any new versions that come out.
Whilst the README is updated oftenly, for the most modern features you might have to check out the [version.md](version.md) since it contains information on the latest patches and updates. 
It also gives you a timeline of the most recent changes (newest -> oldest).

Flags are really important and if not updated here, you can always check out what flag support there is by just running the compiler without anything or running -usage (This will display information such as the usage, the currently supported builds and any flags you might want to use).

As of [0.16A](version.md#0122a) flags consist of:
```
--------------------------------------------
sopl [flags]
     Currently supported targets:
         - nasm_x86_64
     flags:
         -t (target)                                     -> compiles to the given target (default is nasm_x86_64)
         -o (output path)                                -> outputs to that file (example: hello.asm in nasm_x86_64 mode). If the output path is not specified it defaults to the modes default (for nasm_x86_64 thats a.asm)
         (NOT RECOMMENDED - use lighthouse instead) -r   -> runs the program for you if the option is available for that language mode (for example in nasm_x86_64 it calls nasm with gcc to link it to an executeable)
         (NOT RECOMMENDED - use lighthouse instead) -b   -> builds the program for you if the option is available for that language mode
         -i (path to directory to shortcut)              -> Adds a shortcut when including (if it finds the file it automatically expands the path) so you could do -i libs/libc and then just do include "stdio.spl" instead of the whole ordeal you had before
         -release                                        -> builds the program in release mode
         -ntc                                            -> (NoTypeChecking) Disable type checking
         -warn (all, funcs, externs, strings)            -> Enable unused warns for parameter
         -ruf                                            -> Remove unused functions
         -arc (builtin arc)                              -> builds for a builtin architecture
         -arc - (path to custom arc)                     -> builds for a custom architecture following the syntax described in ./examples/arcs
         -usage                                          -> Show this page
--------------------------------------------
```

## Plans
> Source
- [x] externals
- [x] strings
- [x] register access
- [x] includes
- [x] functions
- [x] constants
- [x] variables
- [ ] types
  - [x] char
  - [x] short
  - [x] int
  - [x] long
  - [x] size_t
  - [ ] float
  - [ ] double
- [ ] control flow
  - [x] if statements
  - [x] else statements
  - [x] while statements
  - [ ] else if statements
  - [ ] for statements        -> debating which for loop to implement, C style or iterator like.
- [x] locals
- [x] type checking
  - [x] Function type checking
  - [x] External type checking
- [ ] Optimization
  - [x] Removal of unused strings
  - [x] Removal of unused functions
  - [x] Removal of unused externals
  - [ ] Removal of unused variables
- [ ] casting
  - [x] Casting for constants
  - [ ] Casting for variables
- [ ] Evaluation
  - [x] Variables
  - [x] Conditions
  - [ ] Constants -> it still uses the old "evalutation": i.e. Push stuff onto a virtual stack and pop them off with each operation, which fit the style back then, but now is utterly useless
  - [ ] Function arguments
- [ ] boolean operations || and &&
- [ ] returning values bigger than 8 bytes
- [ ] module system
- [ ] namespaces
- [ ] structs
- [ ] struct methods
- [ ] non-static struct methods
- [ ] interfaces

## Highlighting
Currently SOPL Highlighting only supports vscode but in the future I plan to add support for the majority of the most popular editors i.e.:
- vim
- emacs
- neovim

etc.

For highlighting checkout: [this](https://github.com/Dcraftbg/SOPL-Highlighting) - https://github.com/Dcraftbg/SOPL-Highlighting
## How to use?
*Note that documentation may not cover all of the latest features tho you might expect updates on them shortly after implementation*
*If you want to learn most of the details checkout theversions*

### Hello World!
```sopl
extern "C" printf(*char : int)

func main() {
  printf("Hello World!"c);
}
```

### Locals
- [0.12A](version.md#012a) Added expressions
- [0.11.6A](version.md#0116a) Made local variables be on the stack and merged parameters with variables
- Pre 0.1A Added local variables

**Syntax:**
```sopl
let (name): (type)
```

Local variables are a core part of any programming language. SOPL is no exception. Define a variable like this and then you can use it throughout your code later.
**Example(s)**
```sopl
let a: int = 5+4;
let b: int = 4+3;
if a == b {
  let c: int = a+b;
  printf("a=%d, b=%d, c=%d",a,b,c)
}
else {
  printf("a=%d, b=%d and they are not equal!",a,b)
}
```
### Constants
- [0.11.4A](version.md#0114a) Added types for constants as well as casting
Pre 0.1A

**Syntax:**
```sopl
const (name)<:(type)> = (your constant expersion goes here) ;
```

As in many other languages, Sopl also has a constant system. Constants are expressions that are evaluated at compile time and are therefor very useful. The expression is a set of operations that the program will evaluate. The quickest way to understand how they work is to checkout the examples:

```sopl
const World = "World"
const HelloWorld = "Hello" World +      // Concatenates strings and expressions can use constants
const A = 4;
const B = 5;
const C = A B + ;                       
const HelloWorldNine = HelloWorld C + ; // Concatenates the integers and strings together
```

Everything within the expression must be able to be evaluated at compile time, such as Strings, Integers, Longs and other constants. If you try to use something like a function it would tell you that it doesn't recognize it as a constant value. 
Whilst local variables had a major revamp to their Expression system in 0.12A, constants are still stuck in the old ways of handling things. That is planned to change in the near future

### Expressions
- [0.12A](version.md#012a) Added expressions

Expressions in SOPL are like those in mathematics. We use expressions in all kinds of things like the conditions in if statements or the result of a setoperation.
Any OfP by definition is an Expression of type 'value'. If you add an operation it becomes an 'expression tree'. An example of this is the following:
```sopl
a = b; // b is an Expression of type 'value'
a = b+c*d; // b+c*d is an Expression of type 'expression tree'
```
Expressions are really useful but are also unstable currently if they are too complicated. Most of the expressions you'll see are going to be simple and can easily be evaluated, however if the expression is too complicated the program won't compile and it will require you to put the values under temporary variables - currently there is no recorded case of this happening, however it could very well exist somewhere.

### Including
- [0.16A](version.md#016a) Added the -i flag for making a shortcut with including
- [0.5.1A](version.md#051a) Fixed bug where constants weren't being included 
- Pre 0.1A Added including

Including works in the same way as in other programming languages like C and C++. It evaluates the symbols inside of the file given, and inserts them into the current build. Whilst they are pretty neat, as of today, they can lead to things like:
- include loops
- self including files
etc.
**Syntax**
```spl
include "Path/To/File"
```
Functions.spl:
```spl
extern "C" puts(*char : int)
func sayHello() {
  puts("Hello World!")
}
```
HelloWorld.spl:
```spl
include "./Functions.spl"
func main() {
  sayHello()
}
```

### Functions
- [0.12.1A](version.md#0121a) Added 'result' as an OfP
- [0.11A](version.md#011a) Changed how functions are called with the introduction of argument contracts
- [0.6A](version.md#06a) Changed to now have named parameters
- Pre 0.1A

**Syntax**:
```
func (name) ((contract)) {

}
```
Functions, just like in other languages have a name and a body. In sopl, you have to provide a contract:

A contract is a way to tell the type checker what to expect to have passed to it before calling the function.
The function contract contains:
(Input parameters separated by , : Output parameter)

Some things that should be noted are that you really really really shouldn't be using more than one Output parameter type, since the language doesn't support multiple returns (Outputs were kept this way because of the old x86 days and the return stack).
Inside of functions you can use the "return" keyword to return although it is automatically done for you at the end of the functions declaration that don't have a return type.

**Example(s)**:
```sopl
func sayHello() {
  printf("Hello World!\n"c)
}
func counter(c: long : long) {
  c -= 1;
  counter(c)
  return c;
}
```
As of 0.12A, expression evaluation has been added so the code above does work
As of 0.11A, the 'rs' keyword is planned to be deprecated, and instead replaced with the new syntax and C standard
As of 0.6A, there is a new keyword called 'rs' short for "Return stack"

### Control flow

- [0.13.3A](version.md#013a) Added @goto and @makelabel
- [0.12.2A](version.md#012a) Added while loops
- [0.12A](version.md#012a) Finally settled the syntax of ifs
- [0.11.11A](version.md#01111a) Added 'temporary' ifs
- [0.5A](version.md#05a) Disabled for revamping
- Pre 0.1A Added ifs
- Pre 0.1A Added elses


#### If statements:
**Syntax:**
```sopl
if (condition) {(Body)}
(else) {}
```

**Examples:**
```sopl
let a: int;
let b: int;
let c: bool;
a = 5;
b = 4;
if a==b {
  printf("Nice!"c);
}
else {
  printf("Not nice!"c);
}
```
```sopl
let i: int;
i = 0;
while i < 10 {
  printf("Hello World!"c);
}
```

#### While loops:
**Syntax:**
```sopl
while (condition) {(Body)}
```
**Examples:**
```sopl
func main(){
  let i: int;
  i = 0;
  while i < 5 {
    printf("%d> Hello World!\n",i);
    i+=1;
  }
}
```

#### @makelabel:
Labels are going to be important for the next control flow statement, goto. To define a label, use:
**Syntax:**
```sopl
@makelabel("here goes name of your label as a string")
```

#### @goto:
Now that we have made a label, we can jump to it (if it is within the same function)
**Syntax:**
```sopl
@goto("here goes name of your label as a string")
```
**Examples**
```sopl
func main() {
  let i: int = 0;
  while i < 100 {
    if i == 69 {
      @goto("break")
    }
  }
  @makelabel("break")
}
```
One thing to note: the compiler is still a one pass compiler, collection of labels is done with its own system

### Strings
- [0.12.5A](version.md#0125a)Changed Strings to automatically be CStrings and added s as a way to have sized strings once again - it was too annoying having to type ""c every couple of minutes
- [0.2A](version.md#02a) CStrings 
- Pre 0.1A Strings

In sopl there are 2 different types of strings. There are:
```
cstrings 
strings
```
With the main difference being that cstrings don't push their length after the pointer to the string unlike normal strings.
To define a cstring we use "" (escaping is supported). To define a sized string just add the letter s after the string.

**Examples**

```sopl
func main() {
  printf("Hello World!")
  printf("Foo Bar!")
}
```

### Managing C functions and their return values

SOPL is made to be compatible with the C language and using C functions throughout your code is totally acceptable.
To link with external C funcions you can use the 'extern' keyword.

**Example(s)**:
```sopl
extern "C" fopen(*char, *char);
extern "C" fclose(ptr);
func main(){
  let f: ptr = fopen("hello.txt", "w");
  fwrite("Hello World!", 12, 1, f);
  fclose(f);
}
```



### Extern
- [0.11A](version.md#011a) Changed syntax in 
- [0.7A](version.md#07a) Added Files related in:
  - [stdio.spl](src/C/stdio.spl)
  - [stdlib.spl](src/C/stdlib.spl)
- Added in Pre 0.1A

**Syntax**:
```
extern [TYPE] (name) <(extern contract)>;
```
Extern is a way to access outside features inside the language or link to functions from other languages such as libc from C etc.
It has to be noted here that using extern on its own can sometimes lead to unexpected behavior and
might not be available for the current mode you are running in like if you try to link to standard C functions when you are in JAVA mode.

Thats also a reason why externs are generally not recommended for direct use (although depending on your version it might be required to use them raw (currently its not **exactly** required - use stdio.spl, stdlib.spl directly instead of listing externs every time :D. If you want to implement any C library you can do that and share the information online)).

Externs are really powerful and are generally really good to use for building things that require platform specific functions such as Windows applications etc.

**Example(s)**:

```
extern "C" puts(*char : int); // Links with puts from C
func main(){
  puts("Hello World"c) // uses it to print a string to the console
}
```

### Buffers
Buffers can be declared both on the stack and as global variable. To declare a global buffer you can use the following syntax:
**Syntax:**
```
let name: [<type>,<size>];
```
**Example(s)**:
Taken from [buffers.spl](examples/buffers.spl)
```sopl
include "strlib/strlib.spl"
include "libc/stdio.spl"
let name: [char, 500];
func main(){
  strset(name, 500, 0);
  scanf("%s",name);
  printf("Hello: %s\n",name);
}
```
Re-written with local buffers [name.spl](examples/name.spl):
```sopl
include "strlib/strlib.spl"
include "libc/stdio.spl"
func main(){
  let name: [char, 500];
  strset(name, 500, 0);
  scanf("%s",name);
  printf("Hello: %s\n",name);
}
```
### Interrupts
- [Unknown] Added syscall as a replacement fo interrupt
- [0.4A](version.md#04a) Added interrupts
  
Interrupts are essential for anything you build on platforms such as linux if you don't want to use libc. Whilst they aren't useful as much on windows machines WSL could be used to make linux syscalls into something which can run on windows.

To use interrupts you can do 'interrupt' followed by the number of the interrupt. So syscall (int 128) is 
```sopl
interrupt 128
```

### Registers
- Registers are now officially "deprecated" - using them throughout your code is fine, but it isn't recommended as it may cause a lot of unwanted bugs
- [0.11A](version.md#011a) Added float registers and the last remaining registers.
- Pre 0.1A Added the majority of the registers

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
- [x] RSI
  - [x] ESI
- [x] RDI
  - [x] EDI
- [x] RSP
  - [x] ESP
- [x] RBP
  - [x] EBP

All of their sub types respectfully:
- [x] R8
- [x] R9
- [x] R10
- [x] R11
- [x] R12
- [x] R13
- [x] R14
- [x] R15

- [x] XMM0
- [x] XMM1
- [x] XMM2
- [x] XMM3
- [x] XMM4
- [x] XMM5
- [x] XMM6
- [x] XMM7
- [x] XMM8
- [x] XMM9

**Syntax**
```
(RegisterName) (Op | RegisterName) [RegisterName | Op]
```
**Example(s)**

Currently supported register operations:
```
(REG1) +  (REG2)  -> adds the two registers together 
(REG1) -  (REG2)  -> subtracts the second register from the first register 
(REG1) *  (REG2)  -> multiples the two registers     
(REG1) == (REG2) -> compares two registers 

(REG1) =  (Expression)  -> Moves the value of the expression to REG1
(REG1) += (Expression)  -> Adds the value of the expression to REG1
(REG1) -= (Expression)  -> Subtracts the value of the expression to REG1
(REG1) *= (Expression)  -> Multiplies the value of the expression to REG1
(REG1) /= (Expression)  -> Divides the value of the expression to REG1
```



### Dlls
SOPL has support for dlls. You can import a function from a dll:
```
dll_import "kernel.dll" CreateWindow(...);
```
Or export to a dll:
```
dll_export "mylib.dll" myfunc(...);
```

## Libs

Libaries are very important to any language and SOPL is no exception (Libs can be found in the libs folder). However SOPL, since it supports a wide range of platfroms, has its libraries divided into 3 Groups:
- Platform specific libraries -> found in the folders following the platforms name: i.e. libs/Linux/, libs/Windows etc.
- Target specific libraries -> An example of a target specific library can be anything from libc. They aren't platform specific however they DO NOT compile for platforms like fasm_x86_64-exe (Not **yet** implemented). 
- Independent Libaries -> libraries that don't depend on anything but raw sopl. These can be found into their seperate folders inside the libs folder: libs/strlib, etc.

You can always write your own libary but SOPL also comes with some built-in libraries to help you out on your journey 

### Platform specific libs
Currently supported platform specific libs:
- Linux (Partially implemented but still doesn't have sysfuncs.spl finished yet)
- Windows (Not implemented entirely but has been started)

Platform specific libs can help you make fast programs that also use platform specific things (WinAPI on windows, Syscalls on Linux)

### Target specific libs
With the currently unfinished libs, its almost a necesity to use target specific libs. Its entirely possible to write whole programs without them, however this can prove to be challenging. 
Currently supported target specific libs:
- C:
  - stdio.spl
  - stdlib.spl

## Independent libs

Currently supported independent libs:
- strlib:
  [x] strlib.spl
  [ ] chrlib.spl (Not done because we don't have boolean operations nor chars working yet)
  [ ] strnum.spl - it isn't entirely finished, it requires itostr, but it does have strtoi
- mathlib:
  [ ] mathlib.spl - its still in development, but does have some features like floorSqrt
