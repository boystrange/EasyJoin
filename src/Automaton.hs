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

module Automaton where

import Aux
import Control.Monad (forM_)
import qualified Data.Set as S
import Data.List (nub, nubBy, groupBy, sort)
import System.IO (openFile, hPutStr, IOMode(WriteMode), hClose)

data Transition s l = Transition s l s
data Automaton s l = Automaton [Transition s l]

(≡) :: (Eq s, Eq l) => Transition s l -> Transition s l -> Bool
(≡) (Transition s₁ l₁ s₂) (Transition s₃ l₂ s₄) = s₁ == s₃ && l₁ == l₂ && s₂ == s₄

make :: (Eq s, Eq l) => [Transition s l] -> Automaton s l
make = Automaton . nubBy (≡)

transitions :: Automaton s l -> [Transition s l]
transitions (Automaton ts) = ts

states :: Eq s => Automaton s l -> [s]
states (Automaton ts) = nub $ [ s | Transition s _ _ <- ts ] ++ [ s | Transition _ _ s <- ts ]

labels :: Eq s => Automaton s l -> s -> s -> [l]
labels (Automaton ts) from to = [ l | Transition s₁ l s₂ <- ts, s₁ == from, s₂ == to ]

groupEdges :: (Eq s, Eq l) => (l -> l -> Bool) -> Automaton s l -> Automaton s [l]
groupEdges merge a@(Automaton ts) =
  make [ Transition from ls to
       | from <- ss,
         to <- ss,
         ls <- groupBy merge (labels a from to) ]
  where
    ss = states a

mapStates :: (Eq s, Eq l, Eq s') => (s -> s') -> Automaton s l -> Automaton s' l
mapStates f (Automaton ts) = make [ Transition (f s₁) l (f s₂) | Transition s₁ l s₂ <- ts ]

mapEdges :: (Eq s, Eq l, Eq l') => (l -> l') -> Automaton s l -> Automaton s l'
mapEdges f (Automaton ts) = make [ Transition s₁ (f l) s₂ | Transition s₁ l s₂ <- ts ]

filterTransitions :: (Transition s l -> Bool) -> Automaton s l -> Automaton s l
filterTransitions p (Automaton ts) = Automaton (filter p ts)

labelsFrom :: Eq s => Automaton s l -> s -> [l]
labelsFrom (Automaton ts) s = [ l | Transition s₁ l _ <- ts, s == s₁ ]

reachable :: (Ord s, Ord l) => s -> Automaton s l -> Automaton s l
reachable is a@(Automaton ts) = filterTransitions reachable a
  where
    rset = limit aux (S.singleton is)

    aux sset = sset `S.union` S.fromList [ s₂ | s <- S.toList sset, Transition s₁ _ s₂ <- ts, s == s₁ ]

    reachable (Transition s _ _) = s `S.member` rset

with :: Automaton s l -> (l -> Bool) -> [s]
with (Automaton ts) p = [ s | Transition s l _ <- ts, p l ]

from :: Eq s => Automaton s l -> s -> (l -> Bool) -> [s]
from (Automaton ts) s p = [ s₂ | Transition s₁ l s₂ <- ts, s == s₁, p l ]

dot :: (Eq s) => (s -> String) -> (l -> String) -> (s -> String) -> String -> String -> Automaton s l -> IO ()
dot showNode showLabel nodeStyle className rank a@(Automaton ts) = do
  h <- openFile (className ++ ".dot") WriteMode
  hPutStr h $ "digraph " ++ className ++ " {\n"
  hPutStr h $ "  rankdir=" ++ rank ++ "\n"
  forM_ (states a) (auxNode h)
  forM_ ts (auxEdge h)
  hPutStr h "}\n"
  hClose h
  where
    auxNode h s = hPutStr h $ "  node[style=\"" ++ nodeStyle s ++ "\"]; \"" ++ showState s ++ "\";\n"

    auxEdge h (Transition s₁ l s₂) = hPutStr h $ "  \"" ++ showState s₁ ++ "\" -> \"" ++ showState s₂ ++ "\" [" ++ showLabel l ++ "];\n"

    stateMap = zip (states a) [0..]

    showState s = showNode s ++ "(" ++ show n ++ ")"
      where
        Just n = lookup s stateMap
