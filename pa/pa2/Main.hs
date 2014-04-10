import qualified AES

main = do
    -- putStrLn "hello"
    -- let plain = aesHighlevel cbcKey1 cbcCt1
    -- putStrLn $ encode $ aes
    -- putStrLn $ encode $ aesHighlevel nistKey nistMessage
    putStrLn $ AES.encode $ AES.fips
    -- putStrLn "done"
