import Numeric
import Data.Char
import Data.Bits
import Data.List
import Data.Ord

encoded = [ "315c4eeaa8b5f8aaf9174145bf43e1784b8fa00dc71d885a804e5ee9fa40b16349c146fb778cdf2d3aff021dfff5b403b510d0d0455468aeb98622b137dae857553ccd8883a7bc37520e06e515d22c954eba5025b8cc57ee59418ce7dc6bc41556bdb36bbca3e8774301fbcaa3b83b220809560987815f65286764703de0f3d524400a19b159610b11ef3e"
          , "234c02ecbbfbafa3ed18510abd11fa724fcda2018a1a8342cf064bbde548b12b07df44ba7191d9606ef4081ffde5ad46a5069d9f7f543bedb9c861bf29c7e205132eda9382b0bc2c5c4b45f919cf3a9f1cb74151f6d551f4480c82b2cb24cc5b028aa76eb7b4ab24171ab3cdadb8356f"
          , "32510ba9a7b2bba9b8005d43a304b5714cc0bb0c8a34884dd91304b8ad40b62b07df44ba6e9d8a2368e51d04e0e7b207b70b9b8261112bacb6c866a232dfe257527dc29398f5f3251a0d47e503c66e935de81230b59b7afb5f41afa8d661cb"
          , "32510ba9aab2a8a4fd06414fb517b5605cc0aa0dc91a8908c2064ba8ad5ea06a029056f47a8ad3306ef5021eafe1ac01a81197847a5c68a1b78769a37bc8f4575432c198ccb4ef63590256e305cd3a9544ee4160ead45aef520489e7da7d835402bca670bda8eb775200b8dabbba246b130f040d8ec6447e2c767f3d30ed81ea2e4c1404e1315a1010e7229be6636aaa"
          , "3f561ba9adb4b6ebec54424ba317b564418fac0dd35f8c08d31a1fe9e24fe56808c213f17c81d9607cee021dafe1e001b21ade877a5e68bea88d61b93ac5ee0d562e8e9582f5ef375f0a4ae20ed86e935de81230b59b73fb4302cd95d770c65b40aaa065f2a5e33a5a0bb5dcaba43722130f042f8ec85b7c2070"
          , "32510bfbacfbb9befd54415da243e1695ecabd58c519cd4bd2061bbde24eb76a19d84aba34d8de287be84d07e7e9a30ee714979c7e1123a8bd9822a33ecaf512472e8e8f8db3f9635c1949e640c621854eba0d79eccf52ff111284b4cc61d11902aebc66f2b2e436434eacc0aba938220b084800c2ca4e693522643573b2c4ce35050b0cf774201f0fe52ac9f26d71b6cf61a711cc229f77ace7aa88a2f19983122b11be87a59c355d25f8e4"
          , "32510bfbacfbb9befd54415da243e1695ecabd58c519cd4bd90f1fa6ea5ba47b01c909ba7696cf606ef40c04afe1ac0aa8148dd066592ded9f8774b529c7ea125d298e8883f5e9305f4b44f915cb2bd05af51373fd9b4af511039fa2d96f83414aaaf261bda2e97b170fb5cce2a53e675c154c0d9681596934777e2275b381ce2e40582afe67650b13e72287ff2270abcf73bb028932836fbdecfecee0a3b894473c1bbeb6b4913a536ce4f9b13f1efff71ea313c8661dd9a4ce"
          , "315c4eeaa8b5f8bffd11155ea506b56041c6a00c8a08854dd21a4bbde54ce56801d943ba708b8a3574f40c00fff9e00fa1439fd0654327a3bfc860b92f89ee04132ecb9298f5fd2d5e4b45e40ecc3b9d59e9417df7c95bba410e9aa2ca24c5474da2f276baa3ac325918b2daada43d6712150441c2e04f6565517f317da9d3"
          , "271946f9bbb2aeadec111841a81abc300ecaa01bd8069d5cc91005e9fe4aad6e04d513e96d99de2569bc5e50eeeca709b50a8a987f4264edb6896fb537d0a716132ddc938fb0f836480e06ed0fcd6e9759f40462f9cf57f4564186a2c1778f1543efa270bda5e933421cbe88a4a52222190f471e9bd15f652b653b7071aec59a2705081ffe72651d08f822c9ed6d76e48b63ab15d0208573a7eef027"
          , "466d06ece998b7a2fb1d464fed2ced7641ddaa3cc31c9941cf110abbf409ed39598005b3399ccfafb61d0315fca0a314be138a9f32503bedac8067f03adbf3575c3b8edc9ba7f537530541ab0f9f3cd04ff50d66f1d559ba520e89a2cb2a83"
          , "32510ba9babebbbefd001547a810e67149caee11d945cd7fc81a05e9f85aac650e9052ba6a8cd8257bf14d13e6f0a803b54fde9e77472dbff89d71b57bddef121336cb85ccb8f3315f4b52e301d16e9f52f904"]

decoded = map decode encoded
target = decoded !! (length decoded - 1)

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
sxor ids =
    let values = [decoded !! i | i <- ids]
        mix a b = strxor a b
    in foldl1 mix values

xorchr :: Char -> Char -> Char
xorchr a b = chr $ (ord a) `xor` (ord b)

isGoodChar c = isAlpha c && isAscii c

-- get index of the max element
maxi xs = snd $ maximumBy (comparing fst) (zip xs [0..])

-- return key with highest score
best i keys =
    let scores = map (\k -> score i k) keys
    in keys !! maxi scores

-- apply key to every message at position i
decodePosWithKey i k = map apply decoded where
    apply line = (line !! i) `xorchr` k

-- score of the key is the number of a..zA..Z chars that are produced
-- if the key is applied to all messages at position i
score i k = length $ filter isGoodChar $ decodePosWithKey i k

-- iterate over chars of a pair of messages and select the best key for every
-- position
guess :: [Char] -> Int -> Int -> [Char]
guess key i j =
    let (a, b) = (decoded !! i, decoded !! j)
        (xs, ys) = samelen a b
        xored = strxor a b

        check (i, k, a, e1, e2) | isGoodChar a =
            let k1 = a `xorchr` ' ' `xorchr` e1
                k2 = a `xorchr` ' ' `xorchr` e2
            in best i [k, k1, k2]
        check (i, k, _, _, _) = k

    in map check $ zip5 [0..length xored] key xored xs ys

-- prints only ascii chars, replaces the rest with space
human :: [Char] -> [Char]
human xs = map flt xs where
    flt c | valid c = c
    flt _ = '.'
    valid c = isPrint c || c == ' '

display (i, x) = putStrLn $ hex i ++ " " ++ x

{-
m1 + k = e1
m2 + k = e2
K - ?

e1 + e2 =
  = m1 + k + m2 + k
  = m1 + m2
" " + [a..z] = [A..Z]
" " + [A..Z] = [a..z]

a = any ascii letter a..zA..Z
if e1 + e2 = a =>
m1 + m2 = a =>
m1 = A, m2 + ' ' || m1 = ' ', m2 = A

let m1 = a + ' ' =>
m1 + k = e1 =>
k = e1 + a + ' '
-}

main = do
    let n = length encoded
        last = n - 1

    putStrLn "Encrypted messages displayed as text"
    mapM_ display [(i, human $ decoded !! i) | i <- [0..last]]

    putStrLn ""
    putStrLn "Last message xored with all other messages"
    mapM_ display [(i, human $ sxor [i, last]) | i <- [0..last]]

    let key0 = toChr $ take 1024 $ repeat 0
        mix k (i, j) = guess k i j
        key1 = foldl mix key0 [(i, j) | i <- [0..last], j <- [0..last]]
        key = key1

    -- this is the key that I obtained automatically
--  let key = decode "66396e89c9dbd8cb9874352acd6395102eafce78aa7fed28a06e6bc98d29c50b69b025c919f8aa401a9c6d708f80c066c763fef0123148cdd8e802d05ba98777335daefcecd59c433a6b268b60bf4ef03c9a61"
    -- this is the key that I manually fixed (5 chars)
    let key = decode "66396e89c9dbd8cc9874352acd6395102eafce78aa7fed08a07f6bc98d29c50b69b0339a19f8aa401a9c6d708f80c066c763fef0123148cdd8e802d05ba98777335daefcecd59c433a6b268b60bf4ef03c9a61"
    --                              cc                              08  7f                339a                                                                                              .
    putStrLn ""
    putStrLn "Guessed key (0, last)"
    putStrLn $ "00 " ++ human key
    putStrLn $ encode key

    putStrLn ""
    putStrLn "Decoded messages"
    putStrLn ("-- " ++ (take 83 $ cycle "1234567890"))
    mapM_ display [(i, human $ strxor (decoded !! i) key) | i <- [0..last]]
