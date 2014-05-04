import Data.Char
import Data.Word
import Data.Bits
import Text.Printf
import Network.HTTP
import Numeric


--url = "http://crypto-class.appspot.com/po?er=f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c001bd4a61044426fb515dad3f21f18aa577c0bdf302936266926ff37dbf7035d5eeb4"
url = "http://crypto-class.appspot.com/po?er=f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c002bc"
baseUrl = "http://crypto-class.appspot.com/po?er="
--cipher = "f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c001bd4a61044426fb515dad3f21f18aa577c0bdf302936266926ff37dbf7035d5eeb4"
cipher = concat [ "f20bdba6ff29eed7b046d1df9fb70000"
                , "58b1ffb4210a580f748b4ac714c001bd"
                , "4a61044426fb515dad3f21f18aa577c0"
                , "bdf302936266926ff37dbf7035d5eeb4"]
--"f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c001bd4a61044426fb515dad3f21f18aa577c0bdf302936266926ff37dbf7035d5eeb4"

split _ [] = []
split n xs = a : split n b where
    (a, b) = splitAt n xs

hex :: Word8 -> String
hex x = pad $ showHex x "" where
    pad c | length c == 1 = "0" ++ c
    pad c = c

-- hex string -> [Char]
decode :: String -> [Word8]
decode xs = map (fromIntegral . fst . head . readHex) $ split 2 xs
-- [Char] -> hex string
encode :: [Word8] -> String
encode = concatMap hex

data OracleReply = BadPadding
                 | BadMAC
                 | AllOk
                 | WUT deriving Show

askOracle url = do
    resp <- simpleHTTP (getRequest $ baseUrl ++ url)
    code <- getResponseCode resp
    return $ case code of
        (2, 0, 0) -> AllOk
        (4, 0, 3) -> BadPadding
        (4, 0, 4) -> BadMAC
        otherwise -> WUT

asHex :: Int -> String
asHex i = printf "%02x" i

asInt :: String -> Int
asInt i = fst $ head $ readHex i

lxor xs ys = zipWith xor xs ys

-- makePadding i = replicate i (printf "%02x" i)
makePadding :: Int -> String
makePadding i = concat $ replicate i $ asHex i

guessChar blockpos g decoded =
    let guess = g
        encd = decode cipher

        padding = length decoded + 1
        pads = replicate padding (fromIntegral padding) :: [Word8]
        pos = 16 - padding
        allGuess = guess : decoded
        allPads = replicate pos 0 ++ pads
        zeros = replicate pos 0
        xlist = zeros ++ allGuess

        start = []
        iv = take 16 $ drop (16 * blockpos) encd
        block = take 16 $ drop (16 * (blockpos + 1)) encd

        newIV = iv `lxor` xlist `lxor` allPads
    in encode $ start ++ newIV ++ block

guess blockpos currentSecret i = do
    let char = guessChar blockpos i currentSecret
    --putStrLn char
    --putStrLn $ take (16 * 2) char
    rep <- askOracle char
    -- putStrLn $ show i ++ " " ++ show rep
    return rep

guessSingleChar blockpos currentSecret 255 = error "CANNOT GUESS"
guessSingleChar blockpos currentSecret n = do
    rep <- guess blockpos currentSecret n
    case rep of
        BadMAC -> return (n : currentSecret)
        otherwise -> guessSingleChar blockpos currentSecret (n + 1)

guessBlock blockpos currentSecret | length currentSecret >= 16 = error "BLOCK DONE"
guessBlock blockpos currentSecret = do
    newSecret <- guessSingleChar blockpos currentSecret 0
    putStrLn $ "secret (int)" ++ show newSecret
    putStrLn $ "secret (chr) >" ++ secretStr newSecret ++ "<"
    guessBlock blockpos newSecret

secretStr = map (chr . fromIntegral)

--secret = [84, 104, 101, 32, 77, 97, 103, 105, 99, 32, 87, 111, 114, 100, 115, 32]
secret = [9, 9, 9, 9, 9, 9, 9, 9, 9]

main = do
    --resp <- simpleHTTP (getRequest url)
    --code <- getResponseCode resp
    --putStrLn body
    --let code = rspCode resp
    --putStrLn $ show code
    putStrLn $ show $ length secret
    putStrLn $ secretStr $ secret
    putStrLn $ take (16 * 4) cipher
    --mapM_ guess [0..255]
    guessBlock 2 secret

{-
- How to solve the problem: you basically need to guess 3 CBC random IV
- blocks. First, guess two first blocks (with empty initial secret):
-   guessBlock 0 []
-   guessBlock 1 []
-
- Next, guess third block. First, you need to guess correct padding. To do
- this:
- * disable short-circuit in `guessSingleChar` (comment out BadMAC -> ...)
- * print oracle response in `guess` (putStrLn ... show rep)
- * run `guessBlock 2 []`
-
- You will see BadMAC two times: for 1 and for 9. 9 is the original correct
- padding and you need to use it as initial secret. Undo first two changes
- that are metioned in the previous list (short-circuit and printing)
- and run:
-   guessBlock 2 $ replicate 9 9
-
- There will be your third block.
-}
