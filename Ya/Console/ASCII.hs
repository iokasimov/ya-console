module Ya.Console.ASCII where

import Ya
import Ya.ASCII
import Ya.World

import "base" Data.Bool as Base (Bool (False))
import "base" Data.Char (Char)
import "base" Data.String (IsString (fromString))
import "base" GHC.Err (error)
import "base" GHC.IsList (IsList (Item, toList, fromList))
import "base" GHC.Integer (Integer)
import "base" Text.Show (show)
import "base" System.IO (IO, BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin, putStr, putChar, getChar)

clear :: IO Unit
clear = do
 putStr "\ESC[2J"
 putStr "\ESC[100A"

prepare :: IO Unit
prepare = do
 hSetBuffering stdin NoBuffering
 hSetEcho stdin Base.False
 putStr "\ESC[?25l"

-- type Buffering = Block `S` Line `S` Char

input :: IO ASCII
input = getChar `yo` char_to_ascii

output :: ASCII -> IO ASCII
output character = putChar `hv` ascii_to_char character `yu` character

type Styled = Turn Unit `S` Turn Unit `S` Turn Unit `S` Turn Unit `S` Turn Unit

pattern Emphasize e = This (This (This (This e)))
pattern Invisible e = This (This (This (That e)))
pattern Underline e = This (This (That e))
pattern Blinking e = This (That e)
pattern Crossing e = That e

type Turn e = e `S` e

pattern Off e = This e :: Turn e
pattern On e = That e :: Turn e

styled = putStr `ha____` is @Styled `ho___`  is
 `li_` is `hu`"\ESC[22m" `la` is `hu`"\ESC[1m"
 `la_` is `hu`"\ESC[27m" `la` is `hu`"\ESC[7m"
 `la_` is `hu`"\ESC[24m" `la` is `hu`"\ESC[4m"
 `la_` is `hu`"\ESC[25m" `la` is `hu`"\ESC[5m"
 `la_` is `hu`"\ESC[29m" `la` is `hu`"\ESC[9m"

caret_to_char :: Caret -> Char
caret_to_char = is `hu` '\HT' `la` is `hu` '\LF' `la` is `hu` '\ESC' `la` is `hu` '\BS' `la` is `hu` '\DEL'

bracket_to_char :: Bracket -> Char
bracket_to_char = is `hu` '(' `la` is `hu` '{' `la` is `hu` '<' `la` is `hu` '['
              `la_` is `hu` ')' `la` is `hu` '}' `la` is `hu` '>' `la` is `hu` ']'

punctuation_to_char :: Punctuate -> Char
punctuation_to_char = is `hu` '\"' `la` is `hu` '\'' `la` is `hu` '#' `la` is `hu` '-' `la` is `hu` '@'
 `la` is `hu` '^' `la` is `hu` '_' `la` is `hu` '`' `la` is `hu` '|' `la` is `hu` '~'
 `la` is `hu` '+' `la` is `hu` '*' `la` is `hu` '%' `la` is `hu` '&' `la` is `hu` '$' `la` is `hu` '\\' `la` is `hu` '/'
 `la` is `hu` '.' `la` is `hu` ',' `la` is `hu` ';' `la` is `hu` ':' `la` is `hu` '!' `la` is `hu` '?' `la` is `hu` ' '

upper_latin_to_char :: Latin -> Char
upper_latin_to_char = is `hu` 'A' `la` is `hu` 'B' `la` is `hu` 'C' `la` is `hu` 'D' `la` is `hu` 'E' `la` is `hu` 'F'
 `la` is `hu` 'G' `la` is `hu` 'H' `la` is `hu` 'I' `la` is `hu` 'J' `la` is `hu` 'K' `la` is `hu` 'L'
 `la` is `hu` 'M' `la` is `hu` 'N' `la` is `hu` 'O' `la` is `hu` 'P' `la` is `hu` 'Q' `la` is `hu` 'R'
 `la` is `hu` 'S' `la` is `hu` 'T' `la` is `hu` 'U' `la` is `hu` 'V' `la` is `hu` 'W' `la` is `hu` 'X'
 `la` is `hu` 'Y' `la` is `hu` 'Z'

lower_latin_to_char :: Latin -> Char
lower_latin_to_char = is `hu` 'a' `la` is `hu` 'b' `la` is `hu` 'c' `la` is `hu` 'd' `la` is `hu` 'e' `la` is `hu` 'f'
 `la` is `hu` 'g' `la` is `hu` 'h' `la` is `hu` 'i' `la` is `hu` 'j' `la` is `hu` 'k' `la` is `hu` 'l'
 `la` is `hu` 'm' `la` is `hu` 'n' `la` is `hu` 'o' `la` is `hu` 'p' `la` is `hu` 'q' `la` is `hu` 'r'
 `la` is `hu` 's' `la` is `hu` 't' `la` is `hu` 'u' `la` is `hu` 'v' `la` is `hu` 'w' `la` is `hu` 'x'
 `la` is `hu` 'y' `la` is `hu` 'z'

digit_to_char :: Digit -> Char
digit_to_char = is `hu` '0' `la` is `hu` '1' `la` is `hu` '2' `la` is `hu` '3' `la` is `hu` '4' `la` is `hu` '5' `la` is `hu` '6' `la` is `hu` '7' `la` is `hu` '8' `la` is `hu` '9'

ascii_to_char :: ASCII -> Char
ascii_to_char = is
  `li` lower_latin_to_char
  `la` upper_latin_to_char
 `la_` digit_to_char `ha'he` is
 `la_` bracket_to_char `ha'he` is
  `la` punctuation_to_char `ha'he` is
 `la_` caret_to_char `ha'he` is

char_to_ascii :: Char -> ASCII
char_to_ascii = \case
 '\BS' -> Caret `hv` by (Back `ha` Space)
 '\HT' -> Caret `hv` by Tab
 '\LF' -> Caret `hv` by Newline
 '\ESC' -> Caret `hv` by Escape
 ' ' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Space
 '\DEL' -> Caret `hv` by Delete
 '/' -> Glyph `ha` Symbol `ha` Punctuate `hv` by (Back `ha` Slash)
 '\\' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Slash
 '(' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `li` Round
 ')' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `li` Round
 '{' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `li` Curly
 '}' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `li` Curly
 '<' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `li` Angle
 '>' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `li` Angle
 '[' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `li` Square
 ']' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `li` Square
 '"' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Doublequote
 '\'' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Singlequote
 '.' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Period
 ',' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Comma
 ';' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Semicolon
 ':' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Colon
 '!' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Exclam
 '?' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Question
 '#' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Hash
 '$' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Dollar
 '%' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Percent
 '&' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Ampersand
 '*' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Asterisk
 '+' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Plus
 '-' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Hyphen
 '@' -> Glyph `ha` Symbol `ha` Punctuate `hv` by At
 '^' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Circumflex
 '_' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Underscore
 '`' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Grave
 '|' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Bar
 '~' -> Glyph `ha` Symbol `ha` Punctuate `hv` by Tilde
 'A' -> Glyph `ha` Letter `ha` Upper `hv` by A
 'B' -> Glyph `ha` Letter `ha` Upper `hv` by B
 'C' -> Glyph `ha` Letter `ha` Upper `hv` by C
 'D' -> Glyph `ha` Letter `ha` Upper `hv` by D
 'E' -> Glyph `ha` Letter `ha` Upper `hv` by E
 'F' -> Glyph `ha` Letter `ha` Upper `hv` by F
 'G' -> Glyph `ha` Letter `ha` Upper `hv` by G
 'H' -> Glyph `ha` Letter `ha` Upper `hv` by H
 'I' -> Glyph `ha` Letter `ha` Upper `hv` by I
 'J' -> Glyph `ha` Letter `ha` Upper `hv` by J
 'K' -> Glyph `ha` Letter `ha` Upper `hv` by K
 'L' -> Glyph `ha` Letter `ha` Upper `hv` by L
 'M' -> Glyph `ha` Letter `ha` Upper `hv` by M
 'N' -> Glyph `ha` Letter `ha` Upper `hv` by N
 'O' -> Glyph `ha` Letter `ha` Upper `hv` by O
 'P' -> Glyph `ha` Letter `ha` Upper `hv` by P
 'Q' -> Glyph `ha` Letter `ha` Upper `hv` by Q
 'R' -> Glyph `ha` Letter `ha` Upper `hv` by R
 'S' -> Glyph `ha` Letter `ha` Upper `hv` by S
 'T' -> Glyph `ha` Letter `ha` Upper `hv` by T
 'U' -> Glyph `ha` Letter `ha` Upper `hv` by U
 'V' -> Glyph `ha` Letter `ha` Upper `hv` by V
 'W' -> Glyph `ha` Letter `ha` Upper `hv` by W
 'X' -> Glyph `ha` Letter `ha` Upper `hv` by X
 'Y' -> Glyph `ha` Letter `ha` Upper `hv` by Y
 'Z' -> Glyph `ha` Letter `ha` Upper `hv` by Z
 'a' -> Glyph `ha` Letter `ha` Lower `hv` by A
 'b' -> Glyph `ha` Letter `ha` Lower `hv` by B
 'c' -> Glyph `ha` Letter `ha` Lower `hv` by C
 'd' -> Glyph `ha` Letter `ha` Lower `hv` by D
 'e' -> Glyph `ha` Letter `ha` Lower `hv` by E
 'f' -> Glyph `ha` Letter `ha` Lower `hv` by F
 'g' -> Glyph `ha` Letter `ha` Lower `hv` by G
 'h' -> Glyph `ha` Letter `ha` Lower `hv` by H
 'i' -> Glyph `ha` Letter `ha` Lower `hv` by I
 'j' -> Glyph `ha` Letter `ha` Lower `hv` by J
 'k' -> Glyph `ha` Letter `ha` Lower `hv` by K
 'l' -> Glyph `ha` Letter `ha` Lower `hv` by L
 'm' -> Glyph `ha` Letter `ha` Lower `hv` by M
 'n' -> Glyph `ha` Letter `ha` Lower `hv` by N
 'o' -> Glyph `ha` Letter `ha` Lower `hv` by O
 'p' -> Glyph `ha` Letter `ha` Lower `hv` by P
 'q' -> Glyph `ha` Letter `ha` Lower `hv` by Q
 'r' -> Glyph `ha` Letter `ha` Lower `hv` by R
 's' -> Glyph `ha` Letter `ha` Lower `hv` by S
 't' -> Glyph `ha` Letter `ha` Lower `hv` by T
 'u' -> Glyph `ha` Letter `ha` Lower `hv` by U
 'v' -> Glyph `ha` Letter `ha` Lower `hv` by V
 'w' -> Glyph `ha` Letter `ha` Lower `hv` by W
 'x' -> Glyph `ha` Letter `ha` Lower `hv` by X
 'y' -> Glyph `ha` Letter `ha` Lower `hv` by Y
 'z' -> Glyph `ha` Letter `ha` Lower `hv` by Z
 '0' -> Glyph `ha` Digit `hv` by Zero
 '1' -> Glyph `ha` Digit `hv` by One
 '2' -> Glyph `ha` Digit `hv` by Two
 '3' -> Glyph `ha` Digit `hv` by Three
 '4' -> Glyph `ha` Digit `hv` by Four
 '5' -> Glyph `ha` Digit `hv` by Five
 '6' -> Glyph `ha` Digit `hv` by Six
 '7' -> Glyph `ha` Digit `hv` by Seven
 '8' -> Glyph `ha` Digit `hv` by Eight
 '9' -> Glyph `ha` Digit `hv` by Nine
 _ -> error "Not ASCII!"

instance IsString (List Char) where
 fromString x = T'TT'I (Some (Construct (worker x))) where
  worker (c : []) = Item c `ha` Last `hv` Unit
  worker (c : cs) = Item c `ha` Next `hv` worker cs

-- char_to_ascii_with_error :: Char -> ASCII
-- char_to_ascii_with_error x =
 -- (error ('\'' : x : '\'' : " - is not ASCII") `hs` i)
 -- (char_to_ascii x)

instance IsString (List ASCII) where
 fromString [] = T'TT'I (None Unit)
 fromString x = T'TT'I (Some (Construct (worker x))) where
  worker (c : []) = Item `hv` char_to_ascii c `ha` Last `hv` Unit
  worker (c : cs) = Item `hv` char_to_ascii c `ha` Next `hv` worker cs

-- instance IsString (List Latin) where

instance IsString (Construction Optional ASCII) where
 fromString x = Construct (worker x) where
  worker (c : []) = Item `hv` char_to_ascii c `ha` Last `hv` Unit
  worker (c : cs) = Item `hv` char_to_ascii c `ha` Next `hv` worker cs

instance IsList (Construction Optional item) where
 type Item (Construction Optional item) = item
 fromList x = Construct (worker x) where
  worker (c : []) = Item c `ha` Last `hv` Unit
  worker (c : cs) = Item c `ha` Next `hv` worker cs

-- instance IsList (Construction Optional item) where
 -- type Item (Construction Optional item) = item
 -- fromList x = Construct (worker x) where
  -- worker (c : []) = Last c
  -- worker (c : cs) = Next c (worker cs)

-- TODO: it should be `Nonempty List Digit`
integer :: Integer -> Nonempty List Digit
integer = show `ho` fromList `ho'yo` digit where

 digit '0' = by Zero
 digit '1' = by One
 digit '2' = by Two
 digit '3' = by Three
 digit '4' = by Four
 digit '5' = by Five
 digit '6' = by Six
 digit '7' = by Seven
 digit '8' = by Eight
 digit '9' = by Nine
 digit _ = error "Not a digit!"
