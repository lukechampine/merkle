Merkle
------

[Merkle root](http://en.wikipedia.org/wiki/Merkle_tree) algorithms in various languages.

| language   | status      |
|------------|-------------|
| Go         | Complete    |
| C          | Complete    |
| Haskell    | Not started |

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

Note that all segment sizes larger than the input data will result in equal performance, since only one hash is required. Therefore, such a benchmark can be used to gauge the throughput of the implementation's SHA256 algorithm.

| seg. size | 16        | 64        | 256       | 1024      | 4096     | 2^23     |
|:----------|----------:|----------:|----------:|----------:|---------:|---------:|
| C         | 7.88 MB/s | 24.7 MB/s | 52.4 MB/s | 83.9 MB/s | 105 MB/s | 105 MB/s |
| Go        | 1.66 MB/s | 6.36 MB/s | 24.7 MB/s | 69.9 MB/s | 140 MB/s | 210 MB/s |

