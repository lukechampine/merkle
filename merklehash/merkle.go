// Package merklehash provides a wrapper for the hash.Hash interface, enabling
// it to compute Merkle roots.
package merklehash

import (
	"hash"
	"math/bits"
)

var leafPrefix = []byte{0}
var nodePrefix = []byte{1}

// Tree wraps a hash.Hash to compute Merkle roots.
type Tree struct {
	h     hash.Hash
	buf   []byte
	stack [64][]byte
	used  uint64
}

func (t *Tree) leafHash(leaf []byte) []byte {
	t.h.Reset()
	t.h.Write(leafPrefix)
	t.h.Write(leaf)
	return t.h.Sum(nil)
}

func (t *Tree) nodeHash(dst, left, right []byte) []byte {
	t.h.Reset()
	t.h.Write(nodePrefix)
	t.h.Write(left)
	t.h.Write(right)
	return t.h.Sum(dst)
}

func (t *Tree) appendLeaf(leaf []byte) {
	h := t.leafHash(leaf)
	i := uint64(0)
	for ; t.used&(1<<i) != 0; i++ {
		h = t.nodeHash(h[:0], t.stack[i], h)
	}
	t.stack[i] = h
	t.used++
}

// Write implements hash.Hash.
func (t *Tree) Write(p []byte) (int, error) {
	lenp := len(p)
	for len(p) > 0 {
		n := copy(t.buf[len(t.buf):cap(t.buf)], p)
		t.buf = t.buf[:len(t.buf)+n]
		p = p[n:]
		if len(t.buf) == cap(t.buf) {
			t.appendLeaf(t.buf)
			t.buf = t.buf[:0]
		}
	}
	return lenp, nil
}

// Write implements hash.Hash.
func (t *Tree) Sum(p []byte) []byte {
	if len(t.buf) > 0 {
		t.appendLeaf(t.buf)
		t.buf = t.buf[:0]
	}

	i := bits.TrailingZeros64(t.used)
	root := t.stack[i]
	for i++; i < bits.LeadingZeros64(t.used); i++ {
		if t.used&(1<<i) != 0 {
			root = t.nodeHash(root[:0], t.stack[i], root)
		}
	}
	return root
}

// Write implements hash.Hash.
func (t *Tree) Reset() {
	t.h.Reset()
	t.used = 0
}

// Write implements hash.Hash.
func (t *Tree) Size() int { return t.h.Size() }

// Write implements hash.Hash.
func (t *Tree) BlockSize() int { return cap(t.buf) }

// NewTree returns a Tree wrapping the specified hash function, and using the
// specified leaf size.
func NewTree(h hash.Hash, leafSize int) *Tree {
	return &Tree{
		h:   h,
		buf: make([]byte, 0, leafSize),
	}
}

// verify that Tree implements hash.Hash
var _ hash.Hash = (*Tree)(nil)
