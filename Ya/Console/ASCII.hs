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

-- type Buffering = Block `ML` Line `ML` Char

input :: IO ASCII
input = getChar `yo` char_to_ascii

output :: ASCII -> IO ()
output = ascii_to_char `ho` putChar

type Styled = Turn Unit `ML` Turn Unit `ML` Turn Unit `ML` Turn Unit `ML` Turn Unit

pattern Emphasize e = This (This (This (This e)))
pattern Invisible e = This (This (This (That e)))
pattern Underline e = This (This (That e))
pattern Blinking e = This (That e)
pattern Crossing e = That e

type Turn e = e `ML` e

pattern Off e = This e :: Turn e
pattern On e = That e :: Turn e

styled = putStr `ha____` is @Styled `ho___`  is
 `li_` is `hu`"\ESC[22m" `la` is `hu`"\ESC[1m"
 `la_` is `hu`"\ESC[27m" `la` is `hu`"\ESC[7m"
 `la_` is `hu`"\ESC[24m" `la` is `hu`"\ESC[4m"
 `la_` is `hu`"\ESC[25m" `la` is `hu`"\ESC[5m"
 `la_` is `hu`"\ESC[29m" `la` is `hu`"\ESC[9m"

caret_to_char :: Caret -> Char
caret_to_char = is `hu` '\BS' `la` is `hu` '\HT' `la` is `hu` '\LF' `la` is `hu` '\ESC' `la` is `hu` '\SP' `la` is `hu` '\DEL'

bracket_to_char :: Bracket -> Char
bracket_to_char = is `hu` '(' `la` is `hu` '{' `la` is `hu` '<' `la` is `hu` '['
            `la_` is `hu` ')' `la` is `hu` '}' `la` is `hu` '>' `la` is `hu` ']'

punctuation_to_char :: Punctuate -> Char
punctuation_to_char = is `hu` '\"' `la` is `hu` '\'' `la` is `hu` '#' `la` is `hu` '-' `la` is `hu` '@'
 `la` is `hu` '^' `la` is `hu` '_' `la` is `hu` '`' `la` is `hu` '|' `la` is `hu` '~'
 `la` is `hu` '+' `la` is `hu` '*' `la` is `hu` '%' `la` is `hu` '&' `la` is `hu` '$' `la` is `hu` '\\' `la` is `hu` '/'
 `la` is `hu` '.' `la` is `hu` ',' `la` is `hu` ';' `la` is `hu` ':' `la` is `hu` '!' `la` is `hu` '?'

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

digit_to_char :: Number -> Char
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
	'\BS' -> Caret Backspace
	'\HT' -> Caret Tab
	'\LF' -> Caret Newline
	'\ESC' -> Caret Escape
	'\SP' -> Caret Space
	'\DEL' -> Caret Delete
	'/' -> Glyph `ha` Symbol `ha` Punctuate `ha` Back `ha` Slash `hv` ()
	'\\' -> Glyph `ha` Symbol `ha` Punctuate `ha` Slash `hv` ()
	'(' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `li` Round
	')' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `li` Round
	'{' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `li` Curly
	'}' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `li` Curly
	'<' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `li` Angle
	'>' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `li` Angle
	'[' -> Glyph `ha` Symbol `ha` Bracket `ha` Opened `li` Square
	']' -> Glyph `ha` Symbol `ha` Bracket `ha` Closed `li` Square
	'"' -> Glyph `ha` Symbol `ha` Punctuate `ha` Doublequote `hv` ()
	'\'' -> Glyph `ha` Symbol `ha` Punctuate `ha` Singlequote `hv` ()
	'.' -> Glyph `ha` Symbol `ha` Punctuate `ha` Period `hv` ()
	',' -> Glyph `ha` Symbol `ha` Punctuate `ha` Comma `hv` ()
	';' -> Glyph `ha` Symbol `ha` Punctuate `ha` Semicolon `hv` ()
	':' -> Glyph `ha` Symbol `ha` Punctuate `ha` Colon `hv` ()
	'!' -> Glyph `ha` Symbol `ha` Punctuate `ha` Exclam `hv` ()
	'?' -> Glyph `ha` Symbol `ha` Punctuate `ha` Question `hv` ()
	'#' -> Glyph `ha` Symbol `ha` Punctuate `ha` Hash `hv` ()
	'$' -> Glyph `ha` Symbol `ha` Punctuate `ha` Dollar `hv` ()
	'%' -> Glyph `ha` Symbol `ha` Punctuate `ha` Percent `hv` ()
	'&' -> Glyph `ha` Symbol `ha` Punctuate `ha` Ampersand `hv` ()
	'*' -> Glyph `ha` Symbol `ha` Punctuate `ha` Asterisk `hv` ()
	'+' -> Glyph `ha` Symbol `ha` Punctuate `ha` Plus `hv` ()
	'-' -> Glyph `ha` Symbol `ha` Punctuate `ha` Hyphen `hv` ()
	'@' -> Glyph `ha` Symbol `ha` Punctuate `ha` At `hv` ()
	'^' -> Glyph `ha` Symbol `ha` Punctuate `ha` Circumflex `hv` ()
	'_' -> Glyph `ha` Symbol `ha` Punctuate `ha` Underscore `hv` ()
	'`' -> Glyph `ha` Symbol `ha` Punctuate `ha` Grave `hv` ()
	'|' -> Glyph `ha` Symbol `ha` Punctuate `ha` Bar `hv` ()
	'~' -> Glyph `ha` Symbol `ha` Punctuate `ha` Tilde `hv` ()
	'A' -> Glyph `ha` Letter `ha` Upper `li`A
	'B' -> Glyph `ha` Letter `ha` Upper `li`B
	'C' -> Glyph `ha` Letter `ha` Upper `li`C
	'D' -> Glyph `ha` Letter `ha` Upper `li`D
	'E' -> Glyph `ha` Letter `ha` Upper `li`E
	'F' -> Glyph `ha` Letter `ha` Upper `li`F
	'G' -> Glyph `ha` Letter `ha` Upper `li`G
	'H' -> Glyph `ha` Letter `ha` Upper `li`H
	'I' -> Glyph `ha` Letter `ha` Upper `li`I
	'J' -> Glyph `ha` Letter `ha` Upper `li`J
	'K' -> Glyph `ha` Letter `ha` Upper `li`K
	'L' -> Glyph `ha` Letter `ha` Upper `li`L
	'M' -> Glyph `ha` Letter `ha` Upper `li`M
	'N' -> Glyph `ha` Letter `ha` Upper `li`N
	'O' -> Glyph `ha` Letter `ha` Upper `li`O
	'P' -> Glyph `ha` Letter `ha` Upper `li`P
	'Q' -> Glyph `ha` Letter `ha` Upper `li`Q
	'R' -> Glyph `ha` Letter `ha` Upper `li`R
	'S' -> Glyph `ha` Letter `ha` Upper `li`S
	'T' -> Glyph `ha` Letter `ha` Upper `li`T
	'U' -> Glyph `ha` Letter `ha` Upper `li`U
	'V' -> Glyph `ha` Letter `ha` Upper `li`V
	'W' -> Glyph `ha` Letter `ha` Upper `li`W
	'X' -> Glyph `ha` Letter `ha` Upper `li`X
	'Y' -> Glyph `ha` Letter `ha` Upper `li`Y
	'Z' -> Glyph `ha` Letter `ha` Upper `li`Z
	'a' -> Glyph `ha` Letter `ha` Lower `li`A
	'b' -> Glyph `ha` Letter `ha` Lower `li`B
	'c' -> Glyph `ha` Letter `ha` Lower `li`C
	'd' -> Glyph `ha` Letter `ha` Lower `li`D
	'e' -> Glyph `ha` Letter `ha` Lower `li`E
	'f' -> Glyph `ha` Letter `ha` Lower `li`F
	'g' -> Glyph `ha` Letter `ha` Lower `li`G
	'h' -> Glyph `ha` Letter `ha` Lower `li`H
	'i' -> Glyph `ha` Letter `ha` Lower `li`I
	'j' -> Glyph `ha` Letter `ha` Lower `li`J
	'k' -> Glyph `ha` Letter `ha` Lower `li`K
	'l' -> Glyph `ha` Letter `ha` Lower `li`L
	'm' -> Glyph `ha` Letter `ha` Lower `li`M
	'n' -> Glyph `ha` Letter `ha` Lower `li`N
	'o' -> Glyph `ha` Letter `ha` Lower `li`O
	'p' -> Glyph `ha` Letter `ha` Lower `li`P
	'q' -> Glyph `ha` Letter `ha` Lower `li`Q
	'r' -> Glyph `ha` Letter `ha` Lower `li`R
	's' -> Glyph `ha` Letter `ha` Lower `li`S
	't' -> Glyph `ha` Letter `ha` Lower `li`T
	'u' -> Glyph `ha` Letter `ha` Lower `li`U
	'v' -> Glyph `ha` Letter `ha` Lower `li`V
	'w' -> Glyph `ha` Letter `ha` Lower `li`W
	'x' -> Glyph `ha` Letter `ha` Lower `li`X
	'y' -> Glyph `ha` Letter `ha` Lower `li`Y
	'z' -> Glyph `ha` Letter `ha` Lower `li`Z
	'0' -> Glyph `ha` Number `ha` Zero `hv` ()
	'1' -> Glyph `ha` Number `ha` One `hv` ()
	'2' -> Glyph `ha` Number `ha` Two `hv` ()
	'3' -> Glyph `ha` Number `ha` Three `hv` ()
	'4' -> Glyph `ha` Number `ha` Four `hv` ()
	'5' -> Glyph `ha` Number `ha` Five `hv` ()
	'6' -> Glyph `ha` Number `ha` Six `hv` ()
	'7' -> Glyph `ha` Number `ha` Seven `hv` ()
	'8' -> Glyph `ha` Number `ha` Eight `hv` ()
	'9' -> Glyph `ha` Number `ha` Nine `hv` ()
	_ -> error "Not ASCII!"

instance IsString (List Char) where
 fromString x = T'TT'I (Some (Construct (worker x))) where
  worker (c : []) = Item c `ha` Maybe `hv` Last
  worker (c : cs) = Item c `ha` Maybe `hv` Next (worker cs)

-- char_to_ascii_with_error :: Char -> ASCII
-- char_to_ascii_with_error x =
	-- (error ('\'' : x : '\'' : " - is not ASCII") `hs` i)
	-- (char_to_ascii x)

instance IsString (List ASCII) where
 fromString [] = T'TT'I (None Unit)
 fromString x = T'TT'I (Some (Construct (worker x))) where
  worker (c : []) = Item `hv` char_to_ascii c `ha` Maybe `hv` Last
  worker (c : cs) = Item `hv` char_to_ascii c `ha` Maybe `ha` Next `hv` worker cs

-- instance IsString (List Latin) where

instance IsString (Construction Optional ASCII) where
 fromString x = Construct (worker x) where
  worker (c : []) = Item `hv` char_to_ascii c `ha` Maybe `hv` Last
  worker (c : cs) = Item `hv` char_to_ascii c `ha` Maybe `ha` Next `hv` worker cs

instance IsList (Construction Optional item) where
 type Item (Construction Optional item) = item
 fromList x = Construct (worker x) where
  worker (c : []) = Item c `ha` Maybe `hv` Last
  worker (c : cs) = Item c `ha` Maybe `ha` Next `hv` worker cs

-- instance IsList (Construction Optional item) where
 -- type Item (Construction Optional item) = item
 -- fromList x = Construct (worker x) where
  -- worker (c : []) = Last c
  -- worker (c : cs) = Next c (worker cs)

integer :: Integer -> Nonempty List ASCII
integer = show `ho` fromList `ho'yo` char_to_ascii
