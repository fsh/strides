
# Strides

## Indexing and Slicing with Stride

One thing missing from default Nim is the ability to do strided slicing.

This is the ability to specify a range with a _step_ parameter to select every
*n*th element in the range, or to reverse the "direction" of the range in case
of a negative step.

This package adds the `@:` operator to do just this:

``` nim

import pkg/strides

let text = "hello world"

# Regular slice
assert text[ 2 .. ^3 ] == "llo wor"

# Strided slice with a step of 2.
assert text[ 2 .. ^3 @: 2 ] == "lowr"

# Strided slice with reversed direction.
assert text[ ^3 .. 2 @: -1 ] == "row oll"

# Just the stride, like `xs[::s]` in Python:
assert text[ @: -1 ] == "dlrow olleh"
assert text[ @: 2 ] == "hlowrd"

# Additionally, a third form of length+stride is supported:

# Positive stride works like `xs[:a:s]` in Python:
assert text[ 10 @: 1 ] == "hello worl"
assert text[ 10 @: 2 ] == "hlowr"
assert text[ 10 @: 3 ] == "hlwl"

# Negative stride works like `xs[a::s]` in Python:
assert text[ 10 @: -1 ] == "lrow olleh"
assert text[ 10 @: -2 ] == "lo le"
assert text[ 10 @: -3 ] == "lwlh"
```

These strides can also be used as iterators in lieu of `countdown()` and `countup()` if
they do not contain any `BackwardsIndex`es.

``` nim

let k1 = collect:
  for i in 0 ..< 10 @: 2:
    i

assert k1 == @[0, 2, 4, 6, 8]

let k2 = collect:
  for i in 20 .. -1 @: -7:
    i

assert k2 == @[20, 13, 6, -1]
```

Here's how they relate:

| Nim iterator         | Python `range()`    | Python index   | Nim + Strides  |
|:---------------------|:--------------------|:---------------|----------------|
| `countdown(a, b, s)` | `range(a, b-1, -s)` | `xs[a:b-1:-s]` | `a .. b @: -s` |
| `countup(a, b, s)`   | `range(a, b+1, s)`  | `xs[a:b+1:s]`  | `a .. b @: s`  |

Note that Nim convention is to be end-point inclusive[^1].

[^1]: Unfortunately.


## LinearSegment

There's more to this package than just `@:`.

The _resolved_ type of `StridedSlice` (made with `@:`) is a `LinearSegment`.
_Resolved_ here means when any `BackwardsIndex` or `StrideIndex` has been
translated into actual integers by interpreting them in the context of a length. And a `LinearSegment` is the finite version of a `LinearSequence`.

Check out the generated documentation here for more examples.
