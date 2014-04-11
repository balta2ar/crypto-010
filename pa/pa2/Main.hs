import qualified AES

main = do
    -- let cipher = AES.encode $ AES.fips
    -- putStrLn cipher
    -- let text = AES.encode $ AES.unfips
    -- putStrLn text

    putStrLn $ AES.decodeStr AES.testMessage
    let cipher = AES.encode $ AES.test
    putStrLn cipher
    let text = AES.encode $ AES.untest
    putStrLn $ AES.decodeStr text
