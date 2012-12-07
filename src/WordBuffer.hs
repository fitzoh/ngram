module WordBuffer where

--buffer used to store current words
data WordBuffer = WordBuffer {n_2::String,
                              n_1::String,
                              n_0::String}
eWB = WordBuffer "" "" ""
                              
--string representation of word buffer              
showWords :: WordBuffer -> String
showWords wb = w2 ++ " " ++ w1 ++ " " ++ w0 
        where w2 = n_2 wb
              w1 = n_1 wb
              w0 = n_0 wb

--add a single character to the current word
addChar :: WordBuffer -> Char -> WordBuffer
addChar wb ch = WordBuffer w2 w1 w0
        where w2 = n_2 wb
              w1 = n_1 wb
              w0 = reverse $ ch : reverse ( n_0 wb)

--add new word to buffer pushing back others
advanceWord :: WordBuffer -> String -> WordBuffer
advanceWord wb = WordBuffer w1 w0
        where w1 = n_1 wb
              w0 = n_0 wb

--replace current word with new word
completeWord :: WordBuffer -> String -> WordBuffer
completeWord wb word = WordBuffer w1 word ""
        where w2 = n_2 wb
              w1 = n_1 wb

--advance buffer making current string empty
advanceBuffer :: WordBuffer -> WordBuffer
advanceBuffer wb = advanceWord wb "" 

