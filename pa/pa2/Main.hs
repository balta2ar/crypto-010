import qualified AES

import Data.Word
import Debug.Trace

q1Key = "140b41b22a29beb4061bda66b6747e14"
q1Message = "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"

q2Key = "140b41b22a29beb4061bda66b6747e14"
q2Message = "5b68629feb8606f9a6667670b75b38a5b4832d0f26e1ab7da33249de7d4afc48e713ac646ace36e872ad5fb8a512428a6e21364b0c374df45503473c5242a253"

test2Message = AES.encodeStr "Hello, this is sample message"
test2Iv =  "02938058029384038048302958092344"
test2Key = "54750293847736762343898484843993"

testCtr1Key = "36f18357be4dbd77f050515c73fcf9f2"
testCtr1Message = "69dda8455c7dd4254bf353b773304eec0ec7702330098ce7f7520d1cbbb20fc388d1b0adb5054dbd7370849dbf0b88d393f252e764f1f5f7ad97ef79d59ce29f5f51eeca32eabedd9afa9329"

q3Key = "36f18357be4dbd77f050515c73fcf9f2"
q3Message = "69dda8455c7dd4254bf353b773304eec0ec7702330098ce7f7520d1cbbb20fc388d1b0adb5054dbd7370849dbf0b88d393f252e764f1f5f7ad97ef79d59ce29f5f51eeca32eabedd9afa9329"

q4Key = "36f18357be4dbd77f050515c73fcf9f2"
q4Message = "770b80259ec33beb2561358a9f2dc617e46218c0a53cbeca695ae45faa8952aa0e311bde9d4e01726d3184c34451"

testCtr2Iv = "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"
testCtr2Key = "2b7e151628aed2a6abf7158809cf4f3c"
testCtr2Message = "6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710"

-- testMessage = "6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710"
-- testKey = "2b7e151628aed2a6abf7158809cf4f3c"
-- testIv = "000102030405060708090a0b0c0d0e0f"
-- testExpected = "7649abac8119b246cee98e9b12e9197d5086cb9b507219ee95db113a917678b273bed6b8e3c1743b7116e69e222295163ff1caa1681fac09120eca307586e1a7"

word8ListToWord128 :: [Word8] -> Integer
word8ListToWord128 xs = sum $ zipWith (*) (map (256^) [0..15]) (reverse (map fromIntegral xs))
word128toWord8List :: Integer -> [Word8]
word128toWord8List x = reverse $ makediv x where
    makediv x | x < 256 = [fromIntegral x]
    makediv x = (fromIntegral (x `mod` 256)) : makediv (fromIntegral (x `quot` 256))

runCbcEncrypt iv key message =
    let blocks = AES.split 16 message

        (_, cipher) = foldl encode (iv, []) blocks
        encode (prev, acc) block =
            let encrypted = AES.aesEncrypt key $ AES.xorwords prev block
            in (encrypted, acc ++ encrypted)
    in cipher

cbcEncrypt hexIv hexKey hexMessage =
    let iv = AES.decode hexIv
        message = AES.decode hexMessage
        cipher = runCbcEncrypt iv (AES.decode hexKey) (message ++ pad message)
        pad xs | length xs `mod` 16 == 0 = replicate 16 (fromIntegral 16)
        pad xs = replicate n (fromIntegral n) where
            n = 16 - (length xs `mod` 16)
    in iv ++ cipher

runCbcDecrypt key message =
    let (iv:blocks) = AES.split 16 message

        (_, plain) = foldl decode (iv, []) blocks
        decode (prev, acc) block =
            let decrypted = AES.xorwords prev $ AES.aesDecrypt key block
            in (block, acc ++ decrypted)
    in plain

cbcDecrypt hexKey hexMessage =
    let plain = runCbcDecrypt (AES.decode hexKey) (AES.decode hexMessage)
        l = length plain
        n = fromIntegral $ plain !! (l - 1)
        unpadded = take (l - n) plain
    in unpadded

runCtrEncrypt iv key message =
    let blocks = AES.split 16 message

        (_, cipher) = foldl encode (word8ListToWord128 iv, []) blocks
        encode (prev, acc) block =
            let encrypted = AES.xorwords block $ AES.aesEncrypt key counter
                counter = word128toWord8List prev
            in (prev + 1, acc ++ encrypted)
    in cipher

ctrEncrypt hexIv hexKey hexMessage =
    let iv = AES.decode hexIv
        message = AES.decode hexMessage
        cipher = runCtrEncrypt iv (AES.decode hexKey) message
    in iv ++ cipher

runCtrDecrypt key message =
    let (iv:blocks) = AES.split 16 message

        (_, plain) = foldl decode (word8ListToWord128 iv, []) blocks
        decode (prev, acc) block =
            let decrypted = AES.xorwords block $ AES.aesEncrypt key counter
                counter = word128toWord8List prev
            in (prev + 1, acc ++ decrypted)
    in plain

ctrDecrypt hexKey hexMessage =
    let plain = runCtrDecrypt (AES.decode hexKey) (AES.decode hexMessage)
    in plain

main = do
    -- let cipher = AES.encode $ AES.fips
    -- putStrLn cipher
    -- let text = AES.encode $ AES.unfips
    -- putStrLn text

    -- putStrLn $ AES.decodeStr AES.testMessage
    -- let cipher = AES.encode $ AES.test
    -- putStrLn cipher
    -- let text = AES.encode $ AES.untest
    -- putStrLn $ AES.decodeStr text
    
    -- content <- readFile "plain.txt"
    -- let text = take (length content - 1) content


    -- putStrLn "Original plaintext"
    -- putStrLn testMessage
    -- let cipher = AES.encode $ cbcEncrypt testIv testKey testMessage
    -- putStrLn ""
    -- putStrLn "Ciphertext"
    -- putStrLn cipher

    -- let decrypted = AES.encode $ cbcDecrypt testKey cipher
    -- putStrLn ""
    -- putStrLn "Decrypted"
    -- putStrLn decrypted
    
    -- putStrLn $ AES.decodeStr $ AES.encode $ cbcDecrypt q1Key q1Message
    -- putStrLn $ AES.decodeStr $ AES.encode $ cbcDecrypt q2Key q2Message
    
    -- putStrLn $ AES.decodeStr test2Message
    -- let cipher = AES.encode $ cbcEncrypt test2Iv test2Key test2Message
    -- putStrLn cipher
    -- putStrLn $ AES.decodeStr $ AES.encode $ cbcDecrypt test2Key cipher

    putStrLn $ AES.decodeStr test2Message
    let cipher = AES.encode $ ctrEncrypt test2Iv test2Key test2Message
    putStrLn cipher
    putStrLn $ AES.decodeStr $ AES.encode $ ctrDecrypt test2Key cipher

    -- putStrLn $ AES.decodeStr testCtr2Message
    -- let cipher = AES.encode $ ctrEncrypt testCtr2Iv testCtr2Key testCtr2Message
    -- putStrLn $ show $ AES.split 32 cipher
    -- putStrLn $ show $ AES.split 32 $ AES.encode $ ctrDecrypt testCtr2Key cipher
