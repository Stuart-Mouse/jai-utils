
# Stuart's Jai Utils

A collection of some basic utiltiy procedures that I use throughout some of my other modules.
I apologize for making people download this in order to use my other modules, but hey, maybe you'll find some good nuggets in here for projects of your own.
Maybe in the future I'll figure out how to do some kind of submodule thing...

## Contents

### Reflection

`any.jai` and `type_info.jai` provide functions for handling Any's and Type Info, since many of my modules (dyncall, Lead Sheets, Data Packer) are doing all kinds of dynamic stuff using reflection.
Functions include allocation and initialization of Any's, getting members of structs, and handling pointer Any's.

There are also some special data types (Small_Any and Any_Array) which provide their own convenience functions for a subset of Any types.


`array.jai` also contains functions for dynamic handling of arrays as Any's, allowing you to resize, insert, or reserve on resizeable arrays with runtime type info.
There are also some macros for searching arrays using inlined procedures (intended for use with quick lambdas) or with code insertion.
Sure, you can just write a for loop, but this is marginally more convenient!


### Other Stuff

The other stuff included here is really basic utility functions like a polymorphic memzero, defer_restore, offset_of, cyclic_modulo, ... and other things of that nature.

