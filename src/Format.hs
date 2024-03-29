module Format where
import qualified Data.Text as T
import Data.Char


--format a string into a list of words
formatString::String -> [String]
formatString content =  formatText $ T.pack content

--format text into a list of words
formatText::T.Text -> [String]
formatText text =  words asciiString
        where 
                formatted = T.toLower $ replacePunct text
                asciiString = filter isAscii $ T.unpack formatted
                
--replace all punctuation in text (other than apostrophe) with space
replacePunct :: (T.Text -> T.Text)
replacePunct = T.map rep
        where
                --list of all characters other than apostrophe
                punct :: String
                punct = map chr $ [33..38] ++ [40..64]
                rep :: Char -> Char
                rep x = if  x `elem` punct
                                then ' '
                                else x

