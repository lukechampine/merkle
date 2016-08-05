import os = std.stdio: File, writeln;
import digest = std.digest.sha: sha256Of, toHexString, LetterCase;

enum SEGSIZE = 64;

pure nothrow @safe auto merkleRoot(in ubyte[32][] hashes) {
	if (hashes.length == 1) {
		return hashes[0];
	}
	ulong i = 1;
	while (i * 2 < hashes.length) {
		i *= 2;
	}
	return sha256Of(hashes[0..i].merkleRoot, hashes[i..$].merkleRoot);
}

void main() {
	auto f = os.File("test.dat", "r");
	ubyte[32] hashes[];
	auto buf = new ubyte[SEGSIZE];
	while (!f.eof()) {
		hashes ~= digest.sha256Of(f.rawRead(buf));
	}
	os.writeln(digest.toHexString!(digest.LetterCase.lower)(hashes.merkleRoot));
}
