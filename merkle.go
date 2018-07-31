package main

import (
	"bufio"
	"crypto/sha256"
	"fmt"
	"hash"
	"io"
	"os"
)

const segSize = 64

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
	s.hash.Sum(s.head.sum[:0])
	s.head.height++
}

func copySeg(dst io.Writer, src io.Reader, buf []byte) (written int, err error) {
	n, err := io.ReadFull(src, buf)
	if err != nil && err != io.ErrUnexpectedEOF {
		return n, err
	}
	return dst.Write(buf[:n])
}

func (s *stack) ReadFrom(r io.Reader) (n int64, err error) {
	buf := make([]byte, segSize)
	for {
		s.hash.Reset()
		copied, err := copySeg(s.hash, r, buf)
		n += int64(copied)
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
	r := bufio.NewReader(file)

	stack := New(sha256.New())
	_, err = stack.ReadFrom(r)
	if err != nil {
		fmt.Println(err)
		return
	}

	fmt.Printf("%x\n", stack.Root())
}
