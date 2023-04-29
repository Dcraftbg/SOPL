**(Latest patch versions in github history)**

V:
 **0.0.6.0A**
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

 **0.0.5.3A**
   - Changed compiler build to now include information of const definition declaration, external location declaration and more
   - Added '-nuw' flag for disabling unused warnings when running in release. 
 **0.0.5.2A** 
   - Changed version.txt to version.md for easier reading
 
 **0.0.5.1A** 
   - Fixed including to now include constant definitions. 
 
 **0.0.5A**   
   - Added basic type checking (without any branching!)
   - -ntc (No Type Checking) flag
   - Updated the macros to be more flexible with loc_display()
   - Changed "".to_string() to String::new() in most places
   - added Unused externals for release to be folded and for constant strings that were never used (for constants)
 
 **0.0.4A**   
   - Added interrupts
 
 **0.0.3.1A** 
   - Added usage 
 
 **0.0.3A**   
   - Added optimization for unused strings as well as debug information for assembly
 
 **0.0.2A**
   - Added cstrings,
   - -release flag 
   - documentation for when everything was added up to this point
   - Added release support for weather or not the callstack should be added in nasm build
 
 **0.0.1A**   
   - Added versions.txt, Updated the README and added the Manual
