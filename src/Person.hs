{-# LANGUAGE OverloadedStrings #-}
module Person(jsonPerson, Person(..), jsonPersons, readPerson) where

import qualified Data.Text as T
import Database.SQLite.Simple.FromRow
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (isValidUtf8, toStrict)
import Data.ByteString.Lazy.UTF8 (toString)
import Text.Regex.TDFA
import Data.Char (isSpace)

data Person = Person Int String String

jsonPerson :: Person -> T.Text
jsonPerson (Person i fn ln) = T.pack $ "{\"id\": " ++ show i ++ ", \"firstName\": \"" ++ fn ++ "\", \"lastName\": \"" ++ ln ++"\"}"

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

-- used to make code look cleaner
(@@) :: T.Text -> T.Text -> T.Text
(@@) = T.append

jsonPersons :: [Person] -> T.Text
jsonPersons xs = T.pack "[" @@ go xs @@ T.pack "]"
  where
    go []       = T.empty
    go [x]      = jsonPerson x
    go (x:y:zs) = jsonPerson x @@ T.pack ", " @@ go (y:zs)

regexJsonPerson :: BL.ByteString
regexJsonPerson = "\\`{\"firstName\":\"(.+)\",\"lastName\":\"(.+)\"}\\'"

byteStringToPerson :: BL.ByteString -> Either BL.ByteString Person
byteStringToPerson bs = toPerson ((filter (not . isSpace) . toString $ bs) =~ regexJsonPerson)
  where
    toPerson :: (String,String,String,[String]) -> Either BL.ByteString Person
    toPerson ("",_,"",[fn,ln]) = Right $ Person 0 fn ln
    toPerson _                 = Left "Was not a valid JSON Person"

readPerson :: BL.ByteString -> Either BL.ByteString Person
readPerson bs
  | not . isValidUtf8 . toStrict $ bs = Left "Was not valid UTF8"
  | otherwise                         = byteStringToPerson bs