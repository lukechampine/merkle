import Crypto.Hash
import qualified Data.ByteString.Lazy as B
import Data.Byteable (toBytes)
import Data.ByteString (append)

segmentSize = 64

type Hash = Digest SHA256

joinHash :: Hash -> Hash -> Hash
joinHash a b = hash (append (toBytes a) (toBytes b))

segments :: B.ByteString -> [B.ByteString]
segments bs
	| B.null bs = []
	| otherwise = seg : segments rest where
		(seg, rest) = B.splitAt segmentSize bs

leaves :: B.ByteString -> [Hash]
leaves bs = map hashlazy (segments bs)

merkleRoot :: [Hash] -> Hash
merkleRoot [h] = h
merkleRoot hs  = joinHash (merkleRoot left) (merkleRoot right) where
	(left, right) = splitAt i hs
	i = until (\x -> x*2 >= length hs) (*2) 1

main :: IO ()
main = do
	file <- B.readFile "test.dat"
	print . merkleRoot . leaves $ file
