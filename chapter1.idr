palindrome : Nat -> String -> Bool
palindrome len str =
  let downcased = toLower str
      bigEnough = length str > len
      validPalindrome = downcased == reverse downcased in
      bigEnough && validPalindrome

counts : String -> (Nat, Nat)
counts str =
  let wordCount = length (words str)
      characterCount = length str in
      (wordCount, characterCount)

-- top_ten: Ord a => List a -> List a
-- top_ten = take 10 a

over_length : Nat -> List String -> Nat
over_length len strings =
  length (filter (\x => length x > len) strings)

showPalindrome : String -> String
showPalindrome str =
  show (palindrome 0 str)

main : IO ()
main = repl "\nEnter a string: " showPalindrome
