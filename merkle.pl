use Digest::SHA qw(sha256);
use constant SEGSIZE => 64;

open FILE, "test.dat" or die "Couldn't open file: $!";
binmode FILE;

my @hashes;
while (read FILE, my $segment, SEGSIZE) {
	push @hashes, (sha256 $segment);
}
close FILE;

while ((scalar @hashes) > 1) {
	my @joinedHashes;
	while (my @pair = splice @hashes, 0, 2) {
		push @joinedHashes, $pair[1] ? (sha256 @pair) : $pair[0];
	}
	@hashes = @joinedHashes;
}
print unpack("H*", $hashes[0]), "\n";
