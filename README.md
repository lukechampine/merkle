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

Note that all segment sizes larger than the input data will result in equal performance, since only one hash is required. Therefore, such a benchmark can be used to gauge the basic throughput of the implemention, i.e. how fast it can feed a file into its SHA256 algorithm.

| seg. size | 16             | 64            | 256          | 1024         | 4096         | 2^23         |
|:----------|---------------:|--------------:|-------------:|-------------:|-------------:|-------------:|
| Go        | 8.94 MB/s      | 28.7 MB/s     | 69.9 MB/s    | 117 MB/s     | 140 MB/s     | 210 MB/s     |
| C         | 11.0 MB/s      | 33.6 MB/s     | 87.4 MB/s    | 108 MB/s     | 123 MB/s     | 140 MB/s     |
| Haskell   | 3.08 MB/s      | 16.4 MB/s     | 61.7 MB/s    | 150 MB/s     | **210 MB/s** | **262 MB/s** |
| Rust      | **14.56 MB/s** | **44.6 MB/s** | **105 MB/s** | **155 MB/s** | 191 MB/s     | 175 MB/s     |
| Perl      | 5.37 MB/s      | 18.64 MB/s    | 51.2 MB/s    | 95.3 MB/s    | 110 MB/s     | 120 MB/s     |

In addition, the hash rate of each implementation's SHA256 algorithm was benchmarked by calculating the hash of the string "test.dat" 10 million times in a loop.

|         | time      | hash rate          |
|:--------|-----------|--------------------|
| Go      | 4.45s     | 2.247 MH/s         |
| C       | 4.61s     | 2.168 MH/s         |
| Haskell | 3.66s     | 2.732 MH/s         |
| Rust    | **3.05s** | **3.274 MH/s**     |
| Perl    | 9.95s     | 1.005 MH/s         |

This explains Rust's speed advantage over C, especially with small segment sizes. For example, with seg. size 16, the C implementation took 0.081 seconds longer than the Rust implementation, but the difference in hash rate accounts for half of that difference.
