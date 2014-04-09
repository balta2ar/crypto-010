import Numeric
import Data.Char
import Data.Bits
import Data.List
import Data.Ord

-- | Common methods

-- break the list into blocks of n items
split _ [] = []
split n xs = a : split n b where
    (a, b) = splitAt n xs

-- hex string -> [Char]
decode xs = map (chr . fst . head . readHex) $ split 2 xs
-- [Char] -> hex string
encode xs = concat $ map hex $ toOrd xs

bin x = showIntAtBase 2 intToDigit x ""

hex x = pad $ showHex x "" where
    pad c | length c == 1 = "0" ++ c
    pad c = c

-- cut the longest of two lists
samelen xs ys | length xs > length ys = (take (length ys) xs, ys)
samelen xs ys = (xs, take (length xs) ys)

toOrd = map ord
toChr = map chr

-- 2 char lists (decoded) -> xored [Char] (decoded)
-- XOR two lists, lists must be decoded (NOT hex representation)
strxor a b =
    let (xs, ys) = samelen a b
    in map (\(x, y) -> chr $ x `xor` y) $ zip (toOrd xs) (toOrd ys)

-- xor list of ids from encoded list
-- sxor ids =
--     let values = [decoded !! i | i <- ids]
--         mix a b = strxor a b
--     in foldl1 mix values

xorchr :: Char -> Char -> Char
xorchr a b = chr $ (ord a) `xor` (ord b)

display (i, x) = putStrLn $ hex i ++ " " ++ x

-- | Specific

cbc_key_1 = "140b41b22a29beb4061bda66b6747e14"
cbc_ct1 = "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"


main = do
    putStrLn "hello"
    putStrLn "done"
