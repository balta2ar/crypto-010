import Data.List
import Numeric
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA256 as SHA

blockSize = 1024

encode = concatMap hex where
    hex x = pad $ showHex x "" where
    pad c | length c == 1 = "0" ++ c
    pad c = c

nsplit :: Int -> L.ByteString -> [L.ByteString]
nsplit _ xs | L.null xs = []
nsplit n xs = a : nsplit n b where
    (a, b) = L.splitAt (fromIntegral n) xs

calcHash name = do
    contents <- L.readFile name
    let chunks = reverse $ nsplit blockSize contents
    let hash = foldl' hashBlock L.empty chunks
        hashBlock total chunk = L.fromStrict $ SHA.hashlazy $ L.concat [chunk, total]
    putStrLn $ (encode $ L.unpack hash) ++ " " ++ name

main = getArgs >>= mapM_ calcHash
