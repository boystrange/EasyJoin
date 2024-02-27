{-
┌───────────────────────────────────────────────────────────────────╖
│ This file is part of EasyJoin.                                    ║
│                                                                   ║
│ EasyJoin is free software: you can redistribute it and/or modify  ║
│ it under the terms of the GNU General Public License as published ║
│ by the Free Software Foundation, either version 3 of the License, ║
│ or (at your option) any later version.                            ║
│                                                                   ║
│ EasyJoin is distributed in the hope that it will be useful, but   ║
│ WITHOUT ANY WARRANTY; without even the implied warranty of        ║
│ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU ║
│ General Public License for more details.                          ║
│                                                                   ║
│ You should have received a copy of the GNU General Public License ║
│ along with EasyJoin. If not, see <http://www.gnu.org/licenses/>.  ║
│                                                                   ║
│ Copyright 2019 Luca Padovani                                      ║
╘═══════════════════════════════════════════════════════════════════╝
-}

module Aux where

nothing :: Maybe a -> Bool
nothing Nothing = True
nothing _ = False

limit :: Eq a => (a -> a) -> a -> a
limit f x | x == y = x
          | otherwise = limit f y
  where
    y = f x

choose :: [[a]] -> [[a]]
choose [] = [[]]
choose (xs : xss) = [ x : ys | x <- xs, ys <- choose xss ]

split :: Eq a => a -> [a] -> [[a]]
split x xs = case span (/= x) xs of
               (ys, []) -> [ys]
               (ys, _ : xs') -> ys : split x xs'
