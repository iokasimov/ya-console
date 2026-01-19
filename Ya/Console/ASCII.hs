module Ya.Console.ASCII where

import Ya
import Ya.ASCII
import Ya.World
import Ya.Literal

import "base" Data.Bool as Base (Bool (False))
import "base" System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin, putStr, putChar, getChar)

clear :: World Unit
clear = do
 putStr "\ESC[2J"
 putStr "\ESC[100A"

prepare :: World Unit
prepare = do
 hSetBuffering stdin NoBuffering
 hSetEcho stdin Base.False
 putStr "\ESC[?25l"

-- type Buffering = Block `S` Line `S` Char

input :: World `T'I` Maybe ASCII
input = getChar `yo` char_to_ascii

output :: ASCII `AR` World ASCII
output character = putChar `hc` ascii_to_char character `yu` character

type Styled = Turn Unit `S` Turn Unit `S` Turn Unit `S` Turn Unit `S` Turn Unit `S` Turn Unit

pattern Emphasize e = This (This (This (This (This e))))
pattern Reversing e = This (This (This (This (That e))))
pattern Underline e = This (This (This (That e)))
pattern Blinking e = This (This (That e))
pattern Crossing e = This (That e)
pattern Darkling e = That e

type Turn e = e `S` e

pattern Off e = This e :: Turn e
pattern On e = That e :: Turn e

styled = putStr `ha____` is @Styled `ho___` is
 `hc__` is `hu` "\ESC[22m" `hs` is `hu` "\ESC[1m"
  `hs_` is `hu` "\ESC[27m" `hs` is `hu` "\ESC[7m"
  `hs_` is `hu` "\ESC[24m" `hs` is `hu` "\ESC[4m"
  `hs_` is `hu` "\ESC[25m" `hs` is `hu` "\ESC[5m"
  `hs_` is `hu` "\ESC[29m" `hs` is `hu` "\ESC[9m"
  `hs_` is `hu` "\ESC[22m" `hs` is `hu` "\ESC[2m"
