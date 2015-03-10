Merkle
------

[Merkle root](http://en.wikipedia.org/wiki/Merkle_tree) algorithms in various languages.

| language   | status      |
|------------|-------------|
| Go         | Complete    |
| C          | Not started |
| Haskell    | Not started |

Merkle root specification
-------------------------

A Merkle tree is a tree of hashes. Each node is formed by hashing the concatenation of the nodes below it. The leaves are formed by hashing the input data, split into segments of equal size. The *Merkle root* is the root node of such a tree.

Implementation details
----------------------

The hash used is [SHA-256](http://en.wikipedia.org/wiki/SHA-2).

The segment size is 64 bytes. If the input is not an even multiple of the segment size, the last leaf will be a hash of the trailing bytes.

When the number of segments is not a power of 2, implementations differ as to how the tree should be constructed. Bitcoin, for example, will simply duplicate a segment if it does not have a "sister" to be hashed with. Here, we use the following scheme. Note that no duplication is performed:

```
     ┌───┴──┐       ┌────┴───┐         ┌─────┴─────┐
  ┌──┴──┐   │    ┌──┴──┐     │      ┌──┴──┐     ┌──┴──┐
┌─┴─┐ ┌─┴─┐ │  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐  ┌─┴─┐ ┌─┴─┐ ┌─┴─┐   │
   (5-leaf)         (6-leaf)             (7-leaf)
```

The input data is 2^22 + 32 bytes (4194336 bytes) of zeros. The correct Merkle root for this input is:
```
e9683665a90bd70aabd7705cba57c2be2a4e913a0ca1a14d765497112c178120
```

Benchmarks
----------

Benchmarks are calculating by summing the `user` and `sys` output of `time`.

| implementation | time       |
|:---------------|-----------:|
| Go             | 0.640s     |

