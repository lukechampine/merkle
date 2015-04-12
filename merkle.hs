import Crypto.Hash
import qualified Data.ByteString as B
import Data.Byteable (toBytes)

segmentSize = 64

type Hash = Digest SHA256

joinHash :: Hash -> Hash -> Hash
joinHash a b = hash (B.append (toBytes a) (toBytes b))

segments :: B.ByteString -> [B.ByteString]
segments bs
	| B.null bs = []
	| otherwise = seg : segments rest where
		(seg, rest) = B.splitAt segmentSize bs

merkleRoot :: [Hash] -> Hash
merkleRoot [h] = h
merkleRoot hs  = joinHash (merkleRoot left) (merkleRoot right) where
	(left, right) = splitAt i hs
	i = until (\x -> x*2 >= length hs) (*2) 1

-- note that this implementation reads the entire file into memory
main :: IO ()
main = B.readFile "test.dat" >>= print . merkleRoot . map hash . segments
