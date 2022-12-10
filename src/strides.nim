## =======
## Strides
## =======
##
## This module features several types and functionality related to linear sequences
## and linear indexing.
##
## The most useful from an ergonomics perspective is probably `StridedSlice`_
## which is like a `HSlice` but with a stride (step). This is akin to Python's
## general slicing like `xs[start:stop:step]`, or its `range(start,stop,step)` function.
##
## The primary way to construct strided slices is with the `@:`_ operator, which
## can be applied in several ways.

runnableExamples:

  let greet = "hello world"

  # The obvious way, as a strided slice:
  assert greet[ 2 .. 8 @: 3 ] == "l r"

  # As a prefix: assumes the whole length.
  assert greet[ @: 2 ] == "hlowrd"
  assert greet[ @: -1 ] == "dlrow olleh"

  # As a length + step: shorthand for `0 ..< length` or `length-1 .. 0`
  assert greet[ 5 @: 1 ] == "hello"
  assert greet[ 5 @: 2 ] == "hlo"
  assert greet[ 5 @: -1 ] == "olleh"

  assert greet[greet.len @: 3] == greet[@:3]

##
## .. note:: Negative strides are allowed, so strided slices can actually run backwards.
##
## Given a length (for example of the container being sliced) we can "resolve" a
## `StridedSlice`_ into a `LinearSegment`_ (a finite `LinearSequence`_). A
## `LinearSegment_` consists of the fields `(initial, stride, count)` which
## should all be integers.
##
## There are two ways to resolve a slice: the Nim way (hard slices) and the Python way (soft slices).
##
## Standard Nim behavior is for slices to go out of bounds. For example
## `"test"[0 ..< 20]` will trigger an exception. Whereas in Python
## `"test"[0:20]` will work and simply return `"test"`.
##
## The first behavior is met with the `^!`_ operator which functions very
## similar to `^^`. It merely translates `BackwardsIndex` and `StridedIndex`
## into integers, not doing any bounds checking.
##
## The second way is with the operator `^?`_ which does soft slicing. It works
## the same way as `^!`_ but automatically constrains the slice so it doesn't go
## out of bounds.
##
## By default, and for convenience(?), containers use `^?`_ to mimic Python
## behavior when using strided slices specifically (might change?), but regular
## slices (`HSlice`) still triggers out of bounds.
##

runnableExamples:

  # ^! resolves the strided slice into a linear segment given a length.
  let linseg1 = (1 .. 100 @: 5) ^! 10
  assert linseg1 is LinearSegment
  assert linseg1.len == 20

  # ^? does the same but constrains the linear segment to `0 ..< length`.
  let linseg2 = (1 .. 100 @: 5) ^? 10
  assert linseg2.len == 2 # constrained to only cover [1, 6]

  let text = "aAbBcCdDeEfF"

  # "Hard" slices trigger out of bounds:
  # text[ (0 .. 100 @: 2) ^! 12 ] --> exception!

  # But this works:
  assert text[ (0 .. 100 @: 2) ^? 12 ] == "abcdef"

##
## To use `StridedSlice`_ in a custom container, you would do something like the
## following:
##

runnableExamples:
  type MyList = object

  proc len*(_: MyList): int = 69

  proc `[]`*(lst: MyList, idx: AnyStrided): auto =
    when idx isnot LinearSegment:
      let idx = idx ^? lst.len # or `^!` for slices that trigger out of bounds.

    assert idx is LinearSegment

    # ...

import std/[math, strformat, algorithm]

func `&`*(a: Slice, b: distinct Slice): auto =
  ## Returns the intersection of two `HSlice`s.
  max(a.a, b.a) .. min(a.b, b.b)

func `$`*(a: BackwardsIndex): string =
  ## Missing from standard library?
  &"^{a.int}"

# We want the types to be lightweight so we don't want any nonsense runtime type
# info. We want the segment to compose the sequence, allowing functions of the
# latter to run on the former, but supporting this in 1.6 turns out to be super
# painful.

when (NimMajor, NimMinor) >= (1, 7):
  type
    LinearSequence*[T] {.pure, inheritable.} = object
      ## Represents the linear sequence `initial + stride * k` for a variable `k`.
      ##
      ## One could also think of it as an infinite loop with a linear index
      ## variable.
      initial*: T
      stride*: T

when (NimMajor, NimMinor) < (1, 7):
  import std/macros except last
  # Ugly hack to get around the fact that we need to use a deprecated syntax
  # which doesn't parse at all in Nim 1.7...
  #
  # And this deprecated syntax is needed due to another bug in the Nim compiler:
  # https://github.com/nim-lang/Nim/issues/16653
  #
  macro declTypeHack(): untyped =
    nnkStmtList.newTree(
      nnkTypeSection.newTree(
        nnkTypeDef.newTree(
          nnkPragmaExpr.newTree(
            nnkPostfix.newTree(newIdentNode("*"), newIdentNode("LinearSequence")),
            nnkPragma.newTree(newIdentNode("pure"))
          ),
          nnkGenericParams.newTree(
            nnkIdentDefs.newTree(newIdentNode("T"), newEmptyNode(), newEmptyNode())
          ),
          nnkObjectTy.newTree(
            nnkPragma.newTree(newIdentNode("inheritable")),
            newEmptyNode(),
            nnkRecList.newTree(
              nnkIdentDefs.newTree(
                nnkPostfix.newTree(newIdentNode("*"), newIdentNode("initial")),
                newIdentNode("T"),
                newEmptyNode()
              ),
              nnkIdentDefs.newTree(
                nnkPostfix.newTree(newIdentNode("*"), newIdentNode("stride")),
                newIdentNode("T"),
                newEmptyNode()
              )
            )
          )
        )
      )
    )

  declTypeHack()
  ## .. note:: If you're using Nim 1.6 or older the documentation for `LinearSequence` won't
  ##   show up properly below. The reasons for this is a vortex of pain. To compile
  ##   the type a certain deprecated syntax is neeeded (due to a compiler bug) which
  ##   doesn't compile at all in Nim 1.7, so it's just generated by a macro instead,
  ##   and thus docs are missing. See source.


type
  LinearSegment*[T, I] {.pure.} = object of LinearSequence[T]
    ## A `LinearSequence` with a `count`: a finite linear sequence.
    ##
    ## This is meant to abstracts a general indexing loop, and iterating over it
    ## produces the following loop:
    ##
    ## .. code-block::
    ##   var index = <initial>
    ##   let stop = <initial> + <count> * <stride>
    ##   while index != stop:
    ##     yield index
    ##     index.inc <stride>
    ##
    ## See `StridedSlice` for a different kind of representation.
    ##
    ## Note: `segment[i]` is not bound checked. Can be constrained with
    ## `segment[a .. b]`.
    ##
    count*: I

static:
  assert sizeof(LinearSegment[int, int]) == sizeof(int) * 3

func initLinearSequence*[T](initial, stride: T): LinearSequence[T] {.inline.} =
  result.initial = initial
  result.stride = stride

func initLinearSequence*[T](stride: T): LinearSequence[T] {.inline.} =
  initLinearSequence(0, stride)

func initLinearSegment*[T, I](initial, stride: T; count: I): LinearSegment[T, I] {.inline.} =
  result.initial = initial
  result.stride = stride
  result.count = count

func `$`*(ls: LinearSequence): string =
  &"LinearSequence({ls.stride}*x + {ls.initial})"

func `$`*(seg: LinearSegment): string =
  &"LinearSegment({seg.initial} + {seg.stride} * (0 ..< {seg.count}))"

converter toTuple*[T](ls: LinearSequence[T]): (T, T) =
  (ls.initial, ls.stride)

converter toTuple*[T, I](seg: LinearSegment[T, I]): (T, T, I) =
  (seg.initial, seg.stride, seg.count)

iterator items*[T](ls: LinearSequence[T]): T {.inline.} =
  ## Infinite loop over the linear sequence `ls[0], ls[1], ls[2], ...`.
  var value = ls.initial
  while true:
    yield value
    value.inc ls.stride

iterator items*(seg: LinearSegment): auto {.inline.} =
  ## Yields `seg[0], seg[1], ..., seg[seg.len - 1]`.
  let stop = seg[seg.count]
  var value = seg.initial
  while value != stop:
    yield value
    value.inc seg.stride

iterator pairs*(seg: LinearSegment): auto {.inline.} =
  ## Yields the sequence `(k, seg[k])` for `k in 0 ..< seg.len`.
  var value = seg.initial
  for i in 0 ..< seg.len:
    yield (i, value)
    value.inc seg.stride


func numStrides(stride, first, last, sadj: distinct SomeInteger): auto {.inline.} =
  case stride:
    of 0: raise newException(ValueError, "linear sequence is degenerate")
    of 1: last - first
    of -1: first - last
    else: (last - first + sadj).euclDiv(stride)





type
  StridedSlice*[T, U] = object
    ## Strided slice. Acts like a `HSlice` but with a stride parameter (`s`).
    ##
    ## Inspiration is Python's slices and `range`, though the end point is still
    ## (unfortunately) counted as inclusive in order to remain compatible with
    ## `HSlice`.
    ##
    ## You would normally construct these with the `@:`_ operator and resolve them into
    ## concrete `LinearSegment`s with the `^!`_ operator.
    ##
    ## .. warning:: The stride should never be 0.
    ##
    a*: T
    b*: U
    s*: int

  StridedIndex* = distinct int ## Represents a `@:stride` index.

func initStridedSlice*[T, U](a: T, b: U, s: int): StridedSlice[T, U] {.inline.} =
  StridedSlice[T, U](a: a, b: b, s: s)

func `$`*(ss: StridedSlice): string =
  &"({$ss.a} .. {$ss.b} @: {$ss.s})"

func `$`*(si: StridedIndex): string =
  &"@:{si.int}"

converter toTuple*[T, I](ss: StridedSlice[T, I]): (T, T, I) =
  (ss.a, ss.b, ss.s)

converter toStridedSlice*(seg: LinearSegment): auto =
  initStridedSlice(seg.initial, seg.last, seg.stride)

func toLinearSegment*(ss: StridedSlice): auto {.inline.} =
  ## Converts a *resolved* `StridedSlice`_ to a `LinearSegment`_.
  when ss.a is BackwardsIndex or ss.b is BackwardsIndex:
    {.error "strided slice must be resolved to convert it to a linear segment".}
  let (a, b, s) = ss.toTuple()
  assert s != 0, "stride cannot be zero"
  let c = if s < 0:
            numStrides(s, a, b - 1, 0)
          else:
            numStrides(s, a, b + 1, s - 1)
  initLinearSegment(a, s, c.max(0))



iterator items*(ss: StridedSlice): auto {.inline.} =
  when ss.a isnot SomeInteger or ss.b isnot SomeInteger:
    {.error "strided slice must be resolved to iterate over it".}

  for i in ss.toLinearSegment():
    yield i

type
  AnyStrided* = ## Any type that can commonly be used to slice with a stride.
    StridedSlice or StridedIndex or LinearSegment
  AnyIndexing* = ## Any type that can commonly be used to index an array-like container.
    SomeInteger or BackwardsIndex or HSlice or AnyStrided

func `^!`*(idx: AnyIndexing, length: SomeInteger): auto {.inline.} =
  ## Resolves indexing-like types.
  ##
  ## Given an integer or `BackwardsIndex` this acts like `^^`, but takes the
  ## length directly (as opposed to the container).
  ##
  ## Given a `HSlice` it resolves any contained `BackwardsIndex` to `int`.
  ##
  ## Given a `StridedSlice` or `StridedIndex` it converts it to a `LinearSegment`.
  ##
  ## Slices are *not* constrained to the given length.
  ##
  ## Normally the only use for this operator is in overloading `[]` to implement indexing.
  ##
  when idx is BackwardsIndex:
    length - idx.int
  elif idx is HSlice:
    idx.a ^! length .. idx.b ^! length
  elif idx is StridedIndex:
    let s = idx.int
    let (a, c) = if s < 0:
      (length - 1, numStrides(s, length, 0, 0))
    else:
      (0, numStrides(s, 0, length, s - 1))
    initLinearSegment(a, s, c.max(0))
  elif idx is StridedSlice:
    (idx.a ^! length .. idx.b ^! length @: idx.s).toLinearSegment()
  else: # SomeInteger or LinearSegment
    idx

func `^?`*(idx: AnyIndexing, length: SomeInteger): auto {.inline.} =
  ## Resolves indexing-like types, and constrains slicing types so they cannot
  ## go out-of-bounds.
  ##
  ## This functions much like `^!`_, but slices (`HSlice`, `StridedSlice`,
  ## `SliceIndex`, `LinearSegment`) will automatically be constrained so they
  ## cannot go out-of-bounds.
  ##
  ## Normally the only use for this operator is in overloading `[]` to implement indexing.
  ##
  ## .. note:: The `^?` and `^!` operators have higher precedence than `..`, so
  ##   parenthesis is needed when using them with a literal slice.
  runnableExamples:
    assert (^1) ^? 50 == 49
    assert (0 .. 9) ^? 50 == 0 .. 9

    assert (20 .. 45) ^? 30 == 20 .. 29
    assert (0 .. 99 @: 20) ^? 50 == initLinearSegment(0, 20, 3)
    assert (@: -1) ^? 10 == initLinearSegment(9, -1, 10)

  let resolved = idx ^! length

  when resolved is HSlice:
    resolved.a.max(0) .. resolved.b.min(length - 1)
  elif resolved is LinearSegment:
    resolved[0 ..< length]
  else:
    resolved

func `@:`*[T, U](slice: HSlice[T, U], step: int): StridedSlice[T, U] {.inline.} =
  ## Turns a regular slice into a strided slice.
  ##
  ## .. note:: For slices with negative stride (i.e. right-to-left slices) the
  ##   largest number should be specified first: `100 .. 0 @: -1` represents the
  ##   indices `100, 99, 98, ..., 0`, but `0 .. 100 @: -1` is empty.
  ##
  StridedSlice[T,U](a: slice.a, b: slice.b, s: step)

func `@:`*[T: SomeInteger](e: T, step: int): StridedSlice[T, int] {.inline.} =
  ## Constructs a strided slice with a length and a step.
  ##
  ## The number is taken as a "total length" of a hypothetical `0 ..< L` slice.
  ## Thus it is interpreted as an *exclusive* end point when the stride is positive, and
  ## an exclusive *start point* when stride is negative.
  ##
  ## The number of elements in the strided slice is easy to calculate as
  ## simply `length div step`.
  ##
  runnableExamples:
    assert (12 @: 2) == (0 .. 11 @: 2)
    assert (12 @: -2) == (11 .. 0 @: -2)

    # Note that these differ in more than direction! One former touches only
    # even numbers, the latter only odd numbers.

  assert e >= 0, "invalid length"
  if step >= 0:
    StridedSlice[T, int](a: T(0), b: e.pred, s: step)
  else:
    StridedSlice[T, int](a: e.pred, b: 0, s: step)

func `@:`*(step: int): auto {.inline.} =
  ## Prefix variant of the step operator. Equivalent to applying a stride to
  ## the entire valid length. Returns a `StridedIndex`.
  ##
  assert step != 0, "stride cannot be zero"
  StridedIndex(step)



func maxLT*[T](ls: LinearSequence[T], bound: T): auto {.inline.} =
  ## Finds `k` such that `ls[k]` is the largest value in the sequence less than
  ## `bound`.
  numStrides(ls.stride, ls.initial, bound - 1, 0)

func maxLTE*[T](ls: LinearSequence[T], bound: T): auto {.inline.} =
  ## Finds `k` such that `ls[k]` is the largest value in the sequence less than
  ## or equal to `bound`.
  numStrides(ls.stride, ls.initial, bound, 0)

func minGT*[T](ls: LinearSequence[T], bound: T): auto {.inline.} =
  ## Finds `k` such that `ls[k]` is the least value in the sequence greater than
  ## `bound`.
  numStrides(ls.stride, ls.initial, bound + 1, ls.stride.abs - 1)

func minGTE*[T](ls: LinearSequence[T], bound: T): auto {.inline.} =
  ## Finds `k` such that `ls[k]` is the least value in the sequence greater or
  ## equal to `bound`.
  numStrides(ls.stride, ls.initial, bound, ls.stride.abs - 1)



func len*(seg: LinearSegment): auto {.inline.} =
  ## Number of values in this sequence.
  ##
  ## Synonym for `seg.count`.
  seg.count

func last*(seg: LinearSegment): auto {.inline.} =
  ## The last value in this sequence.
  ##
  ## Synonym for `seg[seg.len - 1]`.
  ##
  ## .. warning:: Invalid if `seg.len == 0`.
  seg.initial + seg.stride * (seg.count - 1)




func `[]`*[T](ls: LinearSequence[T], k: T): auto {.inline.} =
  ## Gives the `k`th value in this sequence.
  ##
  ## Synonym for `ls.initial + ls.stride * k`.
  ##
  ## .. note:: 0-indexed, so the first value is `ls[0]`.
  ##
  ls.initial + ls.stride * k

func `[]`*[T](ls: LinearSequence[T], slice: HSlice): LinearSegment[T, T] {.inline.} =
  let
    ak = ls.minGTE(slice.a)
    bk = ls.maxLT(slice.b + 1)

  assert ls[ak] >= slice.a
  assert ls[bk] < slice.b + 1

  result.initial = ls[min(ak, bk)]
  result.stride = ls.stride
  if result.initial in slice:
    result.count = (ak - bk).abs + 1
  else:
    result.count = 0

func `-`(seg: LinearSegment): auto =
  result = seg
  result.initial = -result.initial
  result.stride = -result.stride

func `[]`*(seg: LinearSegment, slice: HSlice): auto {.inline.} =
  ## Constrains a `LinearSegment` to its overlap with the given slice.
  runnableExamples:
    let seg = initLinearSegment(1, 3, 6) # [1,4,7,10,13,16]

    assert seg[5 .. 20].toStridedSlice == (7 .. 16 @: 3)
    assert seg[-10 .. 0].toStridedSlice == (1 .. -2 @: 3)
    assert seg[10 .. 12].toStridedSlice == (10 .. 10 @: 3)

  if seg.count == 0:
    return seg

  if seg.stride < 0:
    return -(-seg)[ -slice.b .. -slice.a ]

  var seg = seg
  if seg.initial < slice.a:
    let k = seg.minGTE(slice.a)
    seg.initial = seg[k]
    seg.count -= k

  if seg.last > slice.b:
    seg.count = seg.minGT(slice.b)

  if seg.count < 0:
    seg.count = 0
  seg

func `[]`*[T](arr: openArray[T], seg: AnyStrided): seq[T] =
  ## Overload so that `StridedSlice` and `StridedIndex` can be used with arrays
  ## and the `seq` data type.
  runnableExamples:
    assert [1,2,3,4][ @:2 ] == @[1, 3]

  when seg isnot LinearSegment:
    let seg = seg ^? arr.len
  result = newSeqOfCap[T](seg.count)
  for i in seg:
    result.add(arr[i])

func `[]`*(s: string, seg: AnyStrided): string =
  ## Overload so that `StridedSlice` and `StridedIndex` can be used with
  ## strings.
  runnableExamples:
    assert "nefarious"[ 2 .. 0 @: -1 ] == "fen"

  when seg isnot LinearSegment:
    let seg = seg ^? s.len
  result = newStringOfCap(seg.count)
  for i in seg:
    result.add(s[i])

template assImpl(dest, seg, src: untyped): untyped =
  when seg isnot LinearSegment:
    let seg = seg ^? s.len

  if seg.stride == -1:
    var input = input[0 .. ^1]
    input.reverse()
    dest[seg.initial - seg.count + 1 .. seg.initial] = input
  elif seg.stride == 1:
    dest[seg.initial .. seg.initial + seg.count - 1] = input
  else:
    if input.len != seg.count:
      raise newException(ValueError, "source doesn't match the length of the slice")
    for (c, i) in pairs(seg):
      dest[i] = input[c]

func `[]=`*(s: var string, seg: AnyStrided, input: string) =
  ## Overload so that `StridedSlice` and `StridedIndex` can be used with
  ## strings.
  runnableExamples:
    var s = "nefarious"

    s[ 2 .. 0 @: -1 ] = "gerg"
    assert s == "gregarious" # note the reversed order of 'gerg'

    s[@:3] = "xxxx"
    assert s == "xrexarxoux"

  assImpl(s, seg, input)

func `[]=`*[T](s: var seq[T], seg: AnyStrided, input: openArray[T]) =
  ## Overload so that `StridedSlice` and `StridedIndex` can be used with
  ## strings.

  assImpl(s, seg, input)
