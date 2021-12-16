# Type inference

This document describes what you can and can't expect from the type inference
engine.

# Implicit casting
Implicit casting could be confusing to the programmer, so it is disabled by
default. The "autocast" commandline option enables it, in which cast casts
to higher bitwidths will be automatically inserted

# Constants

Integer constant types have two components: the bit width and the
signedness. You can specify the signedness with a `u` in front of the constant,
and the width is specified with angle brackets after. So if you want to write
the number four as an unsigned, 16 bit number, it would be `u4<16>`

You do not have to specify these types, as they can be inferred. However, if you
have a multiplication of constants, one of them will need to be annotated

# Generics

You can parameterize both external modules as well as functions on named
types. You can write down expressions in type variables, though for now the only
supported operation is addition. To do this, the type variables you want to use
are placed in java style <> before the arguments. These type variables
necessarily are always bit widths, and can be referenced in the function body in
other types. They will soon be allowed to be used in bit indexing expressions.
