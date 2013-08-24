-- Copyright © 2013 Bart Massey
-- Demo of Text.Printf.Extensible

import Text.Printf.Extensible

newtype Unary = Unary Int

instance PrintfArg Unary where
  toField (Unary i) ufmt =
    case ufmt of
      FieldFormat {fmtChar = 'U'} ->
        let (s, i') = if i >= 0 then ("", i) else ("-", (-i)) in
        formatString (s ++ replicate i' '*') (ufmt {fmtChar = 's'})
      _ -> 
        formatInt i ufmt

main :: IO ()
main = do
  printf "%s %lld %c %d %U %d\n"
    "hello" (1 :: Int) 'a' 'b' (Unary (-3)) (Unary 5)
