**(Latest patch versions in github history)**

Newest Version: [0.9.2A](#092a)
# Manual:
  - [0.9.2A](#092a)
    - [0.9.1A](#091a)
    - [0.9A](#09a)
  - [0.8A](#08a)
  - [0.7A](#07a)
  - [0.6.2A](#062a)
    - [0.6.1A](#061a)
    - [0.6A](#06a)
  - [0.5.3A](#053a)
    - [0.5.2A](#052a)
    - [0.5.1A](#051a)
    - [0.5A](#05a)
  - [0.4A](#04a)
  - [0.3.1A](#031a)
    - [0.3A](#03a)
  - [0.2A](#02a)
  - [0.1A](#01a)
# Change Log
 ## 0.9.2A
   - HUGE OPTIMIZATION because string system in rust is bad
   - fixed copying the entire string every time you get a single char
   - cut compile time by 18 TIMES
   - Added benchmarks for everything
 ## 0.9.1A
   - Optimized Strings (No benchmarks cuz I forgot :<)
   - Temprarily disabled chars
 ## 0.9A
   - Optimized Lexer Word processing and re-wrote basically the entirety of the lexer that isn't about strings and chars (planned for next update)
   - Huge optimization to Lexer cutting compile time in half as shown in [benchmark](rsc/benchmarks/1-0_9A.log)
   - Using shared references (Rc's) for locations which makes it so there are a lot less clones for locations
   - Preparation for optimization of everything in the near future:
     - strings in Lexer
     - chars in Lexer
     - currentFunction

   - Benchmarking is now officially a constant part of this project and information on performance is going to be shown there
   - Changed versions.md manual to display newest of sub minor versions at the top instead of the old minor version
   - Pin pointed causes of slow downs :D
 ## 0.8A
   - Changed all versions from sub minor to minor (0.0.1A -> 0.1A).
   - Changed all previous instances of sub-minor versions
   - Added Linux folder
   - Added [Linux/syscalls.spl](src/Linux/syscalls.spl)
   - Added [Linux/systable.txt](src/Linux/systable.txt)
   - Improved performance by removing unnecessary clones
   - Started working on deprecating the currentFunction system to improve it and prepare for the near future changes (Inspired by another [project](https://github.com/Dimitar85898/STAP) of mine by the name STAP)
   - Started preparing for optimzation and finishing some TODOs
   - Added -callstack flag for specifying the amount of space needed for localvars.
   - [Benchmarking](rsc/benchmarks/1-0_8A.log) to find out that defining a lot of constants leads to a lot of time taken for compiling (resolved in [0.9A](#09a), // The issue was actually with the lexer and not with the constants). 
     - At 350+ constants it takes roughly 10-8ms to evaluate which is a long time for such a simple thing.
   - Added project [resources](rsc)
    - Added [benchmarks](rsc/benchmarks)

 ## 0.7A 
   - Added C [stdio.spl](src/C/stdio.spl)
   - Added C [stdlib.spl](src/C/stdlib.spl)
   - Added [helloC.spl](examples/helloC.spl) example
   - Added [comments.spl](examples/comments.spl) example
   - Fixed Comments
   - Changed [README.md](README.md) version syntax
   - *Added optimization to remove any strings that were declared inside of functions that didn't end up in the final build
 ## 0.6.2A
   - Added optimization for unused functions
   - Changed the flag -nuw to now be -nou with parameters for (all, funcs, externs, strings) to disable specific warnings from being shown
   - Updated [README.md](README.md#flags-and-versions) flags
 ## 0.6.1A
   - Changed -r to now run the program as well as build it
 ## 0.6A
   - Added named parameters
   - type checking for local variables
   - redid the entire function system to no longer use a callstack to make it more compatible with C
   - Fixed the majority of the errors to do with pop and remade them to use manual instructions I.E. sub rsp, 8 to expand the stack and move in the value by hand
   - Introduced 'rs' keyword for the return stack. Now you can do rs push and then the value (only works for registers currently)
   - Added examples for function parameters: 
    [functions.spl](examples/functions.spl)
    [argstest.spl](examples/argstest.spl)
   - Fixed examples to now use new function syntax as well as cstrings
   - Changed version.md syntax
   - Added new test cases:
    [basicHelloWorld.spl](examples/basicHelloWorld.spl)

 ## 0.5.3A
   - Changed compiler build to now include information of const definition declaration, external location declaration and more
   - Added '-nuw' flag for disabling unused warnings when running in release. 
 ## 0.5.2A 
   - Changed version.txt to version.md for easier reading
 
 ## 0.5.1A 
   - Fixed including to now include constant definitions. 
 
 ## 0.5A   
   - Added basic type checking (without any branching!)
   - -ntc (No Type Checking) flag
   - Updated the macros to be more flexible with loc_display()
   - Changed "".to_string() to String::new() in most places
   - added Unused externals for release to be folded and for constant strings that were never used (for constants)
 
 ## 0.4A   
   - Added interrupts
 
 ## 0.3.1A 
   - Added usage 
 
 ## 0.3A   
   - Added optimization for unused strings as well as debug information for assembly
 
 ## 0.2A
   - Added cstrings,
   - -release flag 
   - documentation for when everything was added up to this point
   - Added release support for weather or not the callstack should be added in nasm build
 
 ## 0.1A   
   - Added versions.txt, Updated the README and added the Manual
