package main

import (
	"crypto/sha256"
	"fmt"
	"hash"
	"io"
	"os"
)

const leafSize = 64 // bytes

type stack struct {
	head *elem
	hash hash.Hash
}

type elem struct {
	height int
	sum    []byte
	next   *elem
}

func (s *stack) pop() (e *elem) {
	e = s.head
	s.head = s.head.next
	return
}

func (s *stack) push(e *elem) {
	e.next = s.head
	s.head = e

	for s.head.next != nil && s.head.height == s.head.next.height {
		s.collapse()
	}
}

func (s *stack) collapse() {
	oldhead := s.pop()
	s.hash.Reset()
	s.hash.Write(s.head.sum)
	s.hash.Write(oldhead.sum)
	s.hash.Sum(s.head.sum[:0]) // reuse head's memory
	s.head.height++
}

func (s *stack) ReadFrom(r io.Reader) (n int64, err error) {
	for {
		s.hash.Reset()
		copied, err := io.CopyN(s.hash, r, leafSize)
		n += copied
		if err != nil && err != io.EOF {
			return n, err
		} else if copied == 0 {
			break
		}
		s.push(&elem{0, s.hash.Sum(nil), nil})
	}
	return n, nil
}

func (s *stack) Root() []byte {
	if s.head == nil {
		return nil
	}

	for s.head.next != nil {
		s.collapse()
	}
	return s.head.sum
}

func New(h hash.Hash) *stack {
	return &stack{hash: h}
}

func main() {
	file, err := os.Open("test.dat")
	if err != nil {
		fmt.Println(err)
		return
	}

	stack := New(sha256.New())
	_, err = stack.ReadFrom(file)
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Printf("%x\n", stack.Root())
}
