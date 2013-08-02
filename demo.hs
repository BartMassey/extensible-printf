-- Copyright © 2013 Bart Massey
-- Demo of Text.Printf.Extensible

import Text.Printf.Extensible

newtype Unary = Unary Int

instance PrintfArg Unary where
  toUPrintf (Unary i) ufmt = 
    case ufmt of
      UFmt {fmtCharacter = 'z'} ->
        uprintString (replicate i '*') (ufmt {fmtCharacter = 's'})
      _ -> 
        uprintInt i ufmt

main :: IO ()
main = do
  printf "%s %d %d %z %d\n" "hello" (1 :: Int) 'a' (Unary 3) (Unary 5)
