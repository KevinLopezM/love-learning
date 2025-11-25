import System.IO (writeFile)
import System.Random (randomRIO)

-- Print huge Integer digit-by-digit
showFull :: Integer -> String
showFull n
  | n < 0 = error "negative"
  | n < 10 = show n
  | otherwise = showFull (n `div` 10) ++ show (n `mod` 10)

-- Remove trailing zeros
stripTrailingZeros :: Integer -> Integer
stripTrailingZeros n = if r == 0 then stripTrailingZeros q else n
  where
    (q, r) = n `divMod` 10

-- Add commas every 3 digits, from the right

addCommas s = case length s of
  0 -> ""
  1 -> s
  2 -> s
  3 -> s
  n -> addCommas (take (n - 3) s) ++ "," ++ drop (n - 3) s

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  putStrLn $ "Wow " ++ name ++ ", you just ran real Haskell from VS Code!"

  n <- randomRIO (100, 999) :: IO Int
  let fact :: Integer
      fact = product [1 .. toEnum n]
      cleanFact = stripTrailingZeros fact
      prettyNumber = addCommas (showFull cleanFact)
      filename = "factorial_pretty.txt"

  -- Save pretty version to file
  writeFile filename prettyNumber

  putStrLn ""
  putStrLn "Here's your random factorial — beautifully formatted:"
  putStrLn $ show n ++ "! = " ++ prettyNumber
  putStrLn $ "→ " ++ show (length $ showFull cleanFact) ++ " meaningful digits"
  putStrLn $ "→ Saved to " ++ filename
  putStrLn "Pure functional elegance — Haskell at its finest!"

-- Learning take aways from this file:
-- Common errors with Windows and Haskell
--  1. emojis not able to print
--  2. stale import using old libraries
--  3. PATH issues with ghc and cabal
--  4. Haskell will not print large numbers, but default to scientific notation
--  5. VScode will sometimes show error even though the program works fine (VScode will cache the error).
--     Restart Haskell or close folder and open again.
-- ===========================================================================
-- Learning Takeaways – AI comments from coding session
-- ===========================================================================
--
-- 1. Unicode & PowerShell: Emojis (U+2728) and fancy dashes (U+2014) crash
--    the default Windows PowerShell terminal due to code-page limits.
--    → Fix: Use plain ASCII (-, ->, <3) or switch to Windows Terminal / PowerShell 7.
--
-- 2. Stale template imports: New `stack new` projects often contain a leftover
--    `import Data.Text.Array (run)` that no longer exists → delete or comment it out.
--
-- 3. Toolchain PATH issues: Commands like `stack`, `ghc`, or `git` may say
--    “not recognized” if the installer didn’t add them to PATH.
--    → Fix: Re-run the installer (choose “Add to PATH”) or manually edit
--       System → Advanced → Environment Variables.
--
-- 4. Printing huge Integers: The default `show` for very large `Integer`s can
--    truncate, show scientific notation, or even display `0`. GHC’s pretty-printer
--    sometimes gives up on numbers > ~2000 digits.
--    → Fix: Write a custom digit-by-digit printer (like `showFull`) or force
--       full output with explicit recursion.
--
-- 5. VS Code red squiggles ≠ real errors: The Haskell Language Server (HLS)
--    caches project info. After adding a new dependency (e.g. `random`), the editor
--    may still underline imports even though `stack run` works fine.
--    → Fix: Ctrl+Shift+P → “Haskell: Restart Haskell Language Server”
--       or simply reload the VS Code window.
--
-- (Bonus lessons we hit later: adding packages to package.yaml, Int vs Integer
--  conversion with `toEnum`, stripping trailing zeros, comma formatting,
--  `stack run --file-watch`, Git setup on Windows, etc.)
-- ===========================================================================