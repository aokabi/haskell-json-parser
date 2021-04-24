module Main where

import Data.Char
import Debug.Trace
import Lib

data Token
  = BBRA
  | EBRA
  | CONMMA
  | COLON
  | STRING String
  | NUMBER Double
  deriving (Show)

data JValue
  = JObject [Member]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
  deriving (Show)

data Member = Member String JValue deriving (Show)

getToken :: String -> (Token, String)
getToken ('{' : xs) = (BBRA, xs)
getToken ('}' : xs) = (EBRA, xs)
getToken (',' : xs) = (CONMMA, xs)
getToken (':' : xs) = (COLON, xs)
getToken ('"' : xs) =
  let (a, b) = getString "" xs
   in (STRING (a), b)
getToken ys@(x : xs)
  | isDigit x =
    let (a, b) = getNumber ys
     in (NUMBER (a), b)
  | otherwise = getToken xs

getNumber :: String -> (Double, String)
getNumber xs =
  let (num_str, json_str) = getNumber' xs
   in (read num_str :: Double, json_str)
  where
    getNumber' :: String -> (String, String)
    getNumber' "" = ("", "")
    getNumber' ys@(x : xs)
      | isDigit x =
        let (ys', xs') = getNumber' xs
         in (x : ys', xs')
      | x == '.' =
        let (ys', xs') = getNumber' xs
         in (x : ys', xs')
      | otherwise = ("", ys)

getString :: String -> String -> (String, String)
getString str (x : xs) =
  case x of
    '"' -> (str, xs)
    _ -> getString (str ++ [x]) xs

lexer :: String -> [Token]
lexer "" = []
lexer s =
  let (t, xs) = getToken s
   in t : (lexer xs)

parser :: [Token] -> JValue
parser ts =
  let (j, ts') = element ts
   in j

object :: [Token] -> (JValue, [Token])
object ts =
  let (m, t : ts') = members ts
   in case t of
        EBRA -> (JObject m, ts')
        _ -> error "no end-brace"

members :: [Token] -> ([Member], [Token])
members ts =
  let (m, ts') = member ts
   in loop [m] ts'
  where
    loop :: [Member] -> [Token] -> ([Member], [Token])
    loop ys [] = (ys, [])
    loop ys zs@(x : xs) =
      case x of
        CONMMA ->
          let (e, ws) = member xs
           in loop (ys ++ [e]) ws
        _ -> (ys, zs)

member :: [Token] -> (Member, [Token])
member (STRING s : COLON : ts) =
  let (e, ts') = element ts
   in (Member s e, ts')

elements :: [Token] -> ([JValue], [Token])
elements ts =
  let (e, ts') = element ts
   in loop [e] ts'
  where
    loop :: [JValue] -> [Token] -> ([JValue], [Token])
    loop ys [] = (ys, [])
    loop ys zs@(x : xs) =
      case x of
        CONMMA ->
          let (e, ws) = element xs
           in loop (ys ++ [e]) ws
        _ -> (ys, zs)

element :: [Token] -> (JValue, [Token])
element ts = value ts

value :: [Token] -> (JValue, [Token])
value (BBRA : ts) = object ts
value (STRING t : ts) = (JString t, ts)
value (NUMBER t : ts) = (JNumber t, ts)
value (t : ts) = (JNull, ts)

json_string =
  "{ \
  \  \"name\": \"takaya\",\
  \  \"age\": 28\
  \ }"

parse_json :: IO ()
parse_json =
  let token = lexer json_string
      parsed = parser token
   in do
        print token
        print parsed

main :: IO ()
main =
  do
    -- print $ getNumber "28}"
    parse_json
