use std::io::prelude::*;
use std::fs::File;

extern crate crypto;
use crypto::digest::Digest;
use crypto::sha2::Sha256;

extern crate rustc_serialize as serialize;
use serialize::hex::ToHex;

const SEGSIZE: usize = 64;

struct Stack<T: crypto::digest::Digest> {
	hash: T,
	elems: Vec<Elem>,
}

struct Elem {
	height: u32,
	sum: [u8;32],
}

impl<T: crypto::digest::Digest> Stack<T> {
	fn new(hash: T) -> Stack<T> {
		Stack{
			hash: hash,
			elems: Vec::new(),
		}
	}

	fn push(&mut self, e: Elem) {
		self.elems.push(e);
		while self.elems.len() >= 2 && self.elems[self.elems.len()-1].height == self.elems[self.elems.len()-2].height {
			self.collapse();
		}
	}

	fn collapse(&mut self) {
		let last = self.elems.len()-1;
		self.hash.reset();
		self.hash.input(&self.elems[last-1].sum);
		self.hash.input(&self.elems[last].sum);
		self.hash.result(&mut self.elems[last-1].sum);
		self.elems[last-1].height += 1;
		self.elems.pop();
	}

	fn read_from(&mut self, file: &mut std::fs::File) {
		let mut buf = Box::new([0u8;SEGSIZE]);
		loop {
			let mut e = Elem{height: 0, sum: [0u8;32]};
			self.hash.reset();
			match file.read(&mut *buf) {
				Ok(0)  => break,
				Ok(n)  => self.hash.input(&buf[0..n]),
				Err(_) => panic!("read failed"),
			}
			self.hash.result(&mut e.sum);
			self.push(e);
		}
	}

	fn root(&mut self) -> String {
		if self.elems.len() == 0 {
			return "empty Stack".to_string();
		}

		while self.elems.len() > 1 {
			self.collapse();
		}

		self.elems.pop().unwrap().sum.to_hex()
	}
}

fn main() {
	let mut file = match File::open("test.dat") {
		Ok(file) => file,
		Err(..)  => panic!("couldn't open test file"),
	};

	let mut s = Stack::new(Sha256::new());
	s.read_from(&mut file);

	println!("{}", s.root());
}
