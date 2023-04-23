**(Latest patch versions in github history)**

V:
 **0.0.5.3A**
 > Changed compiler build to now include information of const definition declaration, external location declaration and more. Added '-nuw' flag for disabling unused warnings when running in release. 
 
 **0.0.5.2A** 
 > Changed version.txt to version.md for easier reading
 
 **0.0.5.1A** 
 > Fixed including to now include constant definitions. 
 
 **0.0.5A**   
 > Added basic type checking (without any branching!), The -ntc (No Type Checking) flag, Updated the macros to be more flexible with loc_display(), Changed "".to_string() to String::new() in most places, added Unused externals for release to be folded and for constant strings that were never used (for constants)
 
 **0.0.4A**   
 > Added interrupts
 
 **0.0.3.1A** 
 > Added usage 
 
 **0.0.3A**   
 > Added optimization for unused strings as well as debug information for assembly
 
 **0.0.2A**
> Added cstrings, the -release flag and documentation for when everything was added up to this point, Added release support for weather or not the callstack should be added in nasm build
 
 **0.0.1A**   
 > Added versions.txt, Updated the README and added the Manual
