> Here you can expect to find anything really interesting or important

# Stack oriented removed
31/05/23 **0.11.3A** 

If you bootup the newest version of SOPL, you will see that some of the code you wrote before the update has been broken, following errors like:
```
(P) Error:(loc): Unexpected intrinsic 'push' after {x}
(P) Error:(loc): Unexpected intrinsic 'pop' after {x}
(P) Error:(loc): Unexpected intrinsic 'top' in argument contract
```
That's because, after a lot of thinking, I have decided to remove stack-oriented out the language. It was a tough decision since its been a core part of the language since its very begining however after the shift in meta, and the movement to modern x64 syntax the old ways of stack-oriented are no longer as useful (x64 has its own standards that are related to passing arguments). 

Before (in the x86 era) the only way of passing arguments to functions was to push them onto the stack in reversed order before calling the function. So for example:
modern x64:
```
printf("Hello %ld"c,5l);
```
old x86:
```
5l
"Hello %ld"c
printf
```
I won't get too in-dept onto why this change had to be made, but basically x64 has different standards for passing the arguments and the old x86 is not gonna cut it.

## How does this syntax relate to the removal of stack-oriented?

If you look into the majority of the examples throughout the time prior to [0.11A](version.md#011a) you will see that stack oriented was almost never used, and even if it was somehow used it was for something that could easily be done using modern syntax. It also made it really difficult to follow the project (something could be pushed onto the stack at some point and then used 500 lines later). Another thing that really impacted the usage rate of the stack-oriented part of the language was the change to the more modern C calling conventions in [0.6A](version.md#06a) which basically disabled the access of data on the stack inside of the function. 

## Why?
Why did you make the decision?

Stack-oriented cluttered a lot of the code inside of the compiler making it hard to get around and it was almost always as something that didn't really 'fit' the entire astetic of the language.
I have written 'libraries' for the language, using the syntax I **want** the language to have and the stack was always something that was 'out of place'. 
And probably the biggest reason which was also why I created the dev branch in the first place - scope typechecking. Whilst simple at first glance with normal scopes, it quickly starts getting problematic when you try to expand the logic out:
Lets say you add ifs - well thats pretty simple, its the exact same logic as for the normal scope typechecking - no data should be left on the stack.
Now lets say you add else - oh, now it starts getting complicated, what if you wanted to push some value on the stack if one condition is true, else push another value:
Well now you have to mindful of the values that are pushed by if and else and you have to spend computational power on checking if they are correct and exactly equal.
And now lets say you add while - now there is even more logic to be implemented to be able to know exactly how many things you expect by the end of the loop
And all of this seemed like too much work for too little yield and would make adding a new scope harder and harder which would also make the compile time slower and slower. 
Mind that 70% of everything going on in typechecking was dedicated to the typestack and typelogic, even with ifs elses and whiles not being implemented! - the other 30% mainly dedicated in the return stack (another type of typestack) and the argument passing.
Another reason was the fact that I am planning to switch variables from the CALLSTACK to the normal stack and also add local variables to scopes (it would work for functions (at the top level of the function just allocate enough space for the local variables) however for the scopes it wouldn't since you might have some data already on the stack and if you try and allocate the space at the top level of the scope you won't be able to access the stack).
Now with the stack-oriented part removed the language has become a whole of a lot simpler and a lot more intuitive which was exactly what I was aiming for. Now with that out of the way

## How does this effect the SOPL language
Well... If you have been heavily relying on the stack-oriented part of SOPL (which congrats btw! you need a lot of persistence and patience to do so since the whole system is broken) then that means that a lot of your project will have to be restructered. I am hopping that some time soon I will be able to release some new syntax which would make your life a heck of a lot easier and would make it more neat to do stuff.

Also another thing to note:
TRY TO AVOID USING REGISTERS! - I will talk about this whenever I need to make **the** change but try to avoid the usage of registers because I might be deprecating them soon (or putting them under something)

