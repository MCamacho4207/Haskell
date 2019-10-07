
-- Simple Decode function
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage s = convertBitString( parseSize2( getBitString( s )))

getBitString :: String -> String
getBitString encoded = [x | x <- encoded, (x=='1') || (x=='0')]

parseSize2 :: String -> [String]
parseSize2 [] = []
parseSize2 (x:[y]) = [x:[y]]
parseSize2 (s:str) =  (s:[head str]) : parseSize2 (tail str) 

convertBitString :: [String] -> String
convertBitString [] = []
convertBitString ["00"] = "a"
convertBitString ["01"] = "b"
convertBitString ["10"] = "c"
convertBitString ["11"] = "d"
convertBitString strList = convertBitString ([head strList]) ++ convertBitString (drop 1 strList)

example :: String
example = "H0W 1S 0UR F0UND1NG J1M"