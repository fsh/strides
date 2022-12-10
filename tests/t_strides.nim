import std/strformat
import std/unittest
import std/random
import std/sequtils
import std/sugar


import strides

suite "strides slices":

  test "string":

    let greet = "hello world"

    # As a prefix: assumes the whole length.
    check greet[ @: 2 ] == "hlowrd"
    check greet[ @: -1 ] == "dlrow olleh"

    # As a strepped slice:
    check greet[ 2 .. 8 @: 3 ] == "l r"

    # As a length + step.
    check greet[ 5 @: 1 ] == "hello"
    check greet[ 5 @: 2 ] == "hlo"
    check greet[ 5 @: -1 ] == "olleh"

  test "lenient":
    let greet = "hello world"

    check greet[ ^2 .. 100 @: 1 ] == "ld"

  test "openarray":
    let nums = (0 .. 20).toSeq()
    check nums[1 .. 20 @: 7] == @[ 1, 8, 15 ]

  test "slice <-> segment":
    let seg = initLinearSegment(1, 3, 6) # [1,4,7,10,13,16]

    check seg.toStridedSlice == (1 .. 16 @: 3)

    check seg[5 .. 20].toStridedSlice == (7 .. 16 @: 3)
    check seg[-10 .. 0].toStridedSlice == (1 .. -2 @: 3)
    check seg[10 .. 12].toStridedSlice == (10 .. 10 @: 3)

    check ((1 .. 20 @: 7) ^! 0).toSeq() == @[ 1, 8, 15 ]
    check ((1 .. 20 @: 7) ^? 0).toSeq().len == 0

  test "intro":

    let text = "hello world"

    # Regular slice
    check text[ 2 .. ^3 ] == "llo wor"

    # Strided slice with a step of 2.
    check text[ 2 .. ^3 @: 2 ] == "lowr"

    # Strided slice with reversed direction.
    check text[ ^3 .. 2 @: -1 ] == "row oll"

    # Just the stride, like `xs[::s]` in Python:
    check text[ @: -1 ] == "dlrow olleh"
    check text[ @: 2 ] == "hlowrd"

    # Additionally, a third form of length+stride is supported:

    # Positive stride works like `xs[:a:s]` in Python:
    check text[ 10 @: 1 ] == "hello worl"
    check text[ 10 @: 2 ] == "hlowr"
    check text[ 10 @: 3 ] == "hlwl"

    # Negative stride works like `xs[a::s]` in Python:
    check text[ 10 @: -1 ] == "lrow olleh"
    check text[ 10 @: -2 ] == "lo le"
    check text[ 10 @: -3 ] == "lwlh"

  test "iterators":

    let k1 = collect:
      for i in 0 ..< 10 @: 2:
        i

    check k1 == @[0, 2, 4, 6, 8]

    let k2 = collect:
      for i in 20 .. -1 @: -7:
        i

    check k2 == @[20, 13, 6, -1]

suite "linear sequence":
  setup:
    randomize( 0xdead_decade_dead )
    var lss = @[ initLinearSequence(0, 1),
                 initLinearSequence(-2, -2),
                 initLinearSequence(0, 3),
                 initLinearSequence(0, -3),
                 initLinearSequence(-1, 17),
                 initLinearSequence(77, -7),
                 initLinearSequence(-2, 2),
                 initLinearSequence(10, -5), ]

    for _ in 1 .. 10:
      lss.add(initLinearSequence( rand(-10 .. 10), rand(1 .. 20) ))
      lss.add(initLinearSequence( rand(-10 .. 10), rand(-20 .. -1) ))

  test "index":
    for ls in lss:
      for i in -5 .. 5:
        check ls[i] == ls.stride * i + ls.initial

  test "bounds":
    for ls in lss:
      for i in -10 .. 10:
        let k = ls.maxLT(i)
        check ls[k] < i
        check ls[k+1] >= i or ls[k+1] < ls[k]
        check ls[k-1] >= i or ls[k-1] < ls[k]

      for i in -10 .. 10:
        let k = ls.minGTE(i)
        check ls[k] >= i
        check ls[k+1] < i or ls[k+1] > ls[k]
        check ls[k-1] < i or ls[k-1] > ls[k]

  proc randslice(reach: int): auto =
    let l = rand(-reach .. reach)
    let u = rand(-reach .. reach)
    min(l, u) .. max(l, u)

  test "slicing":
    for ls in lss:
      for _ in 1 .. 10_000:
        let r1 = randslice(200)

        let seg = ls[r1]
        check seg is LinearSegment
        let (start, stride, count) = seg.toTuple()

        check count >= 0
        if count > 0:
          check start in r1
          check start + (count - 1) * stride in r1
          check start + count * stride notin r1
          check start - stride notin r1
        else:
          check r1.len < ls.stride.abs

        let r2 = randslice(150)
        let lim = seg[r2]

        check lim.len <= seg.len
        if lim.len > 0:
          if seg.initial in r2:
            check lim.initial == seg.initial
            if seg.last in r2:
              check lim.initial == seg.initial
              check lim.count == seg.count

          check lim.initial in r2
          check lim.last in r2
        else:
          check (r1 & r2).len < ls.stride.abs

suite "other":
  test "$":
    check $(@:2) == "@:2"
    check $(^2) == "^2"
    check $(^1 .. 0 @: -5) == "(^1 .. 0 @: -5)"
