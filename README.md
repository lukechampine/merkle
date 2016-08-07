Merkle
------

[Merkle root](http://en.wikipedia.org/wiki/Merkle_tree) algorithms in various languages.

| language   | status      |
|------------|-------------|
| Go         | Complete    |
| C          | Complete    |
| Haskell    | Complete    |
| Rust       | Complete    |
| Perl       | Complete    |
| D          | Complete    |

Merkle root specification
-------------------------

A Merkle tree is a tree of hashes. Each node is formed by hashing the concatenation of the nodes below it. The leaves are formed by splitting the input data into segments and hashing each segment. The *Merkle root* is the root node of such a tree.

Implementation details
----------------------

The hash used is [SHA-256](http://en.wikipedia.org/wiki/SHA-2).

All segments are the same size. If the input is not an even multiple of the segment size, the last leaf will be a hash of the trailing bytes.

When the number of segments is not a power of 2, implementations differ as to how the tree should be constructed. Bitcoin, for example, will simply duplicate a segment if it does not have a "sister" to be hashed with. Here, we use the following scheme. Note that no duplication is performed:

```
     ┌───┴──┐       ┌────┴───┐         ┌─────┴─────┐
  ┌──┴──┐   │    ┌──┴──┐     │      ┌──┴──┐     ┌──┴──┐
┌─┴─┐ ┌─┴─┐ │  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐   │
   (5-leaf)         (6-leaf)             (7-leaf)
```

The input data is 2^22 + 32 bytes (4194336 bytes) of zeros. For a leaf size of 64 bytes, the correct Merkle root of this input is:
```
e9683665a90bd70aabd7705cba57c2be2a4e913a0ca1a14d765497112c178120
```

Benchmarks
----------

Benchmarks are given for various segment sizes (in bytes). The throughput is calculated by dividing the size of the input by the execution time, which is obtained by summing the `user` and `sys` outputs of the `time` utility.

Note that all segment sizes larger than the input data will result in equal performance, since only one hash is required. Therefore, such a benchmark can be used to gauge the basic throughput of the implementation, i.e. how fast it can feed a file into its SHA256 algorithm.

| seg. size | 16             | 64            | 256          | 1024         | 4096         | 2^23         |
|:----------|---------------:|--------------:|-------------:|-------------:|-------------:|-------------:|
| Go        | 8.94 MB/s      | 28.7 MB/s     | 69.9 MB/s    | 117 MB/s     | 140 MB/s     | 210 MB/s     |
| C         | 11.0 MB/s      | 33.6 MB/s     | 87.4 MB/s    | 108 MB/s     | 123 MB/s     | 140 MB/s     |
| Haskell   | 3.08 MB/s      | 16.4 MB/s     | 61.7 MB/s    | 150 MB/s     | **210 MB/s** | **262 MB/s** |
| Rust      | **14.56 MB/s** | **44.6 MB/s** | **105 MB/s** | **155 MB/s** | 191 MB/s     | 175 MB/s     |
| Perl      | 5.37 MB/s      | 18.64 MB/s    | 51.2 MB/s    | 95.3 MB/s    | 110 MB/s     | 120 MB/s     |
| D         | 3.36 MB/s      | 10.13 MB/s    | 23.8 MB/s    | 36.2 MB/s    | 41.1 MB/s    | 43.2 MB/s    |

In addition, the hash rate of each implementation's SHA256 algorithm was benchmarked by calculating the hash of the string "test.dat" 10 million times in a loop.

|         | time      | hash rate          |
|:--------|-----------|--------------------|
| Go      |  4.45s    | 2.247 MH/s         |
| C       |  4.61s    | 2.168 MH/s         |
| Haskell |  3.66s    | 2.732 MH/s         |
| Rust    | **3.05s** | **3.274 MH/s**     |
| Perl    |  9.95s    | 1.005 MH/s         |
| D       | 15.19s    | 0.658 MH/s         |

This explains Rust's speed advantage over C, especially with small segment sizes. For example, with seg. size 16, the C implementation took 0.081 seconds longer than the Rust implementation, but the difference in hash rate accounts for half of that difference.

Language Commentary
-------------------

Each implementation is simple enough that it should be readable without
comments. Nevertheless, a high-level description of each implementation is
provided below, along with commentary on the traits of each language.

**Go:**

Go is the language I know best, so I wrote this implementation first. Segments
are read incrementally in a "stack," whose elements each contain a hash and a
height. When a new element is added, its height is compared to the topmost
stack element. If the heights match, the elements are combined: their hashes
are concatenated and re-hashed, and the new element's height is incremented.
The check is then repeated with the new element. For example, if the stack
contains elements with heights [1, 2, 3, 4], then adding an element with
height 1 would trigger a combination of 1 and 1 (forming 2), then 2 and 2
(forming 3), etc. until only one 5 remains. This process is called
"collapsing." When the final Merkle root is requested, elements are combined
without regard to their height. This algorithm is used for the C and Rust
implementations as well. While it is more complicated than a naive approach,
it consumes much less memory.

The stack is written such that it can use any hashing algorithm. It also
reuses the memory of existing elements during the collapsing process. However,
this could be further improved by releasing "consumed" elements to a
`sync.Pool`, and reusing them in `ReadFull` instead of allocating a new
element for each segment.

Go is a nice language to work with, and I think this implementation is pretty
readable. Being able to print the hex string via `fmt` is more convenient than
importing a hex library. I like that the `hash.Sum` function writes to the
provided byte slice, allowing for easy optimization. Note that the `ReadFrom`
method satisfies the `io.ReaderFrom` interface, though there's no real benefit
to doing so.

**C:**

My C is a bit rusty. (No, that isn't a pun.) The implementation is basically a
port of the Go code, with less robust error handling. There are a lot of
things I take for granted when programming in Go, such as multiple return
values, zeroed memory, garbage collection, and a unified `.` dereference
syntax. And regardless of your opinions on OOP, the ability to define methods
on types does wonders for readability! Having to write a loop for `printHash`
was annoying as well.

I included a SHA-256 implementation instead of using a library like openssl.
This may negatively impact performance, but I doubt the effect is significant.
I also had to remember to compile with `-O2`.

It would be interesting to apply libcello to this code. I'm not sure how
dramatically the code would change. And I'm not sure it would qualify as C
anymore. The purpose of this project is to provide readable implementations,
not to make use of exotic libraries, however compelling they are.

**Haskell:**

The Haskell implementation is both concise and blazing fast, almost certainly
because it reads the entire file into memory. (I fiddled with making it lazy,
but in vain.) This approach lends itself well to a functional approach: the
file is split into segments, each segment is hashed, and finally the list
of hashes is processed by `merkleRoot`, which recursively joins hashes.

Splitting the file was more annoying than I anticipated; I was sure there
would be some library function to do it. But no, I was left to implement it
myself using `splitAt`. Calculating the split-point in `merkleRoot` was also a
bit tricky: I use `until` to find the first suitable power of 2. I was tempted
to define a new binary operator for joining hashes, but decided that this
would hurt readability too much.

I was impressed by the `Crypto.Hash` library. Once I defined a local `Hash`
type as `Digest SHA256`, all my functions "just worked," because Haskell was
able to infer from the type signature which version of `hash` to use. And
conveniently, the `Show` instance of `Hash` was just what I wanted (lowercase
hex), so no special formatting was necessary. I did have to wrestle with the
type system a few times though, mostly when dealing with `ByteStrings`.

Haskell's very-high-level semantics make it difficult to optimize, but I was
surprised to find that I didn't need to. As noted earlier, the strict I/O
probably helped a great deal, though it makes this implementation unsuitable
for large files. The SHA256 implementation is also very fast, second only to
Rust's.

**Rust:**

The Rust implementation is again modeled on the Go solution, with a generic
`Stack` type that accepts any hash that satisfies `crypto::digest::Digest`. I
had a difficult time wrapping my head around Rust's complex syntax, type
system, and memory semantics. This was made even more depressing by the
knowledge that whatever I learned could easily be outdated within a few
months. (In truth, I'm not sure if this code even compiles anymore, and I'm
afraid to try.) It also bugged me that I needed to create a whole folder
structure and "Cargo" file to manage such a simple project.

Placating Rust's "borrow checker" is tough for a newbie, requiring a good
knowledge of all the relevant syntax. One of the worst aspects for me was
dealing with string types. Where else but Rust would you see such baffling
expressions as `"empty Stack".to_string()`? The error messages are pretty
helpful in this respect, though. I also like the ability to pattern-match, and
certain macros like `try!`, which would certainly clean up my Go code a bit.

One divergence from the Go implementation was the use of a vector instead of a
linked list for the `Stack` type. I'm not sure how this affected performance,
but readability suffered somewhat. On the whole, the Rust implementation was
blazing fast, in part due to its optimized SHA-256 library. Seeing the
benchmarks made me feel like the effort invested was worth it. Zero-cost
abstractions like the borrow checker are really satisfying when they pay off.

My biggest gripe with Rust is that it feels like a "Computer Science" language
rather than a "Software Engineering" language. Throughout its development, the
Rust team has added lots of fancy features and lots of new syntax, but without
regard to the effect on learning curve, maintainability, stability, technical
debt, etc. Still, I'd love to see Rust displace C++ as the go-to language when
garbage collection is off the table.

**Perl:**

The Perl implementation is the most concise, which should come as no surprise.
It is similar to the Haskell implementation in that it keeps all hashes in
memory, but at least it doesn't store the entire file. As such, it too is
unsuitable for large files, but only when the segment size is small.

I haven't written Perl in a while, but I've written enough to know that my
implementation could definitely be made both shorter and faster -- though
readability might suffer. In particular, I dislike the temporary
`@joinedHashes` array. I'd prefer to use `splice` to join the hashes in-place,
but I shied away because the index math seemed daunting. Perhaps another day.
Using `unpack` to print hex is a bit odd, but perhaps it feels natural to Perl
programmers.

Perl suffers in the benchmarks because it is interpreted, whereas the other
languages are compiled. Its SHA256 implementation is 2-3x slower than the
others, which may account for much of the difference.

**D:**

D has a lot going on. Unwilling to invest the effort required to discover the
"Zen of D," I opted to take the simplest approach possible, mimicking the
Haskell implementation. As a result, the code is quite concise and
inefficient.

Most D functions are bafflingly flexible, accepting multiple arguments, types,
and extra configuration parameters. Fortunately, the compiler's error messages
were pretty helpful. I like the ability to declare pure functions and const
arguments, the ability to make assertions about a functions inputs and
outputs, and the ability to write unit tests alongside the code. Despite this,
D is clearly living in the past to some degree: functions can only return a
single value, leading to C-style "out" parameters (for which D even has a
special keyword!).

The biggest surprise was how slow D's SHA-256 library is. It's almost 5x
slower than the other languages. At first I wondered if the awful performance
was my fault, but the built-in profiler and benchmarking tools revealed that
no, the hash function is to blame. I'm glad I didn't waste too much time
trying to optimize my implementation.

I think I understand now why D is not very popular. It certainly seems
preferable to C++, but the few clear improvements are drowning in a sea of new
features, new syntax, and new terminology. The language is simply too busy,
and lacks a guiding principle. "The most dangerous enemy of a better solution
is an existing codebase that is just good enough."
