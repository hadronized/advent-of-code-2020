module Main where

import Data.List (tails)

(🍍) = tails
(🍕) = (+)
(🧁) = (*)
(🍄) = (,)
(🤐) = zip
(😬) = zip3
(🖋) = print
(🤯) = head
(🍁) :: (Functor f) => (a -> b) -> f a -> f b
(🍁) = fmap
(🧻) = read
(⚟) = lines
(📂) = readFile

main :: IO ()
main = do
  numbers <- (🍁) (((🍁) (🧻)) . (⚟)) ((📂) "input.txt")
  (🖋 ). (🤯) $ [a 🧁 b | x <- (🍍) numbers, y <- (🍍) x, (a, b) <- x 🤐 y, a 🍕 b == 2020]
  (🖋 ). (🤯) $ [a 🧁 b 🧁 c | x <- (🍍) numbers, y <- (🍍) x, z <- (🍍) y, (a, b, c) <-(😬) x y z, a 🍕 b 🍕 c == 2020]
