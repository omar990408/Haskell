findWords :: String -> String -> Int
findWords wor sentence = length[ str| str<- words sentence, str == wor]