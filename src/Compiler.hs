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

module Compiler where

import Control.Monad (forM_, when, unless)
import Debug.Trace
import Data.List ((\\), sort, intercalate, groupBy, nub)
import Data.Maybe (mapMaybe, catMaybes)

import Aux
import Source
import Type
import qualified Automaton as A

--------------------
-- MULTIPLICITIES --
--------------------

class (Eq a, Ord a) => MessageCounter a where
  (⊕) :: a -> a -> a
  (⊖) :: a -> a -> [a]
  (⊔) :: a -> a -> a
  (⊑) :: a -> a -> Bool
  star :: a -> a

--------------
-- COUNTERS --
--------------

data Counter = Exactly Int
             | AtLeast Int
             deriving (Eq, Ord)

instance Show Counter where
  show (Exactly m) = show m
  show (AtLeast m) = show m ++ "⁺"

showCounterPaper :: Counter -> String
showCounterPaper (Exactly m) = show m
showCounterPaper (AtLeast _) = "$"

instance MessageCounter Counter where
  (⊕) (Exactly m) (Exactly n) = Exactly (m + n)
  (⊕) (Exactly m) (AtLeast n) = AtLeast (max m n)
  (⊕) (AtLeast m) (Exactly n) = AtLeast (max m n)
  (⊕) (AtLeast m) (AtLeast n) = AtLeast (max m n)

  (⊖) c           (Exactly 0) = [c]
  (⊖) (Exactly m) (Exactly 1) | m >= 1 = [ Exactly (m - 1) ]
  (⊖) (Exactly _) (Exactly 1) = []
  (⊖) (AtLeast m) (Exactly 1) | m >= 1 = [ Exactly (m - 1), AtLeast m ]

  (⊔) (Exactly m) (Exactly n) = Exactly (max m n)
  (⊔) (Exactly m) (AtLeast n) = AtLeast (max m n)
  (⊔) (AtLeast m) (Exactly n) = AtLeast (max m n)
  (⊔) (AtLeast m) (AtLeast n) = AtLeast (max m n)

  (⊑) (Exactly m) (Exactly n) = m <= n
  (⊑) (Exactly m) (AtLeast n) = m <= n
  (⊑) (AtLeast _) (Exactly _) = False
  (⊑) (AtLeast m) (AtLeast n) = m <= n

  star (Exactly 0) = Exactly 0
  star (Exactly m) = AtLeast m
  star (AtLeast m) = AtLeast m

-------------
-- VECTORS --
-------------

data Vector = Vector [Counter]
  deriving (Eq, Ord)

instance MessageCounter Vector where
  (⊕) (Vector cs₁) (Vector cs₂) = Vector (zipWith (⊕) cs₁ cs₂)
  (⊖) (Vector cs₁) (Vector cs₂) = map Vector (choose (zipWith (⊖) cs₁ cs₂))
  (⊔) (Vector cs₁) (Vector cs₂) = Vector (zipWith (⊔) cs₁ cs₂)
  (⊑) (Vector cs₁) (Vector cs₂) = all (uncurry (⊑)) (zip cs₁ cs₂)
  star (Vector cs) = Vector (map star cs)

instance Show Vector where
  show (Vector cs) = intercalate "," (map show cs)

showVectorPaper :: Vector -> String
showVectorPaper (Vector cs) = concat (map showCounterPaper cs)

vectore :: Vector
vectore = Vector []

vector0 :: Signature -> Vector
vector0 = Vector . map (const $ Exactly 0)

vector :: Signature -> (Int -> Counter) -> Tag -> Vector
vector σ c tag = Vector (map (\(tag', _) -> if tag == tag' then c 1 else Exactly 0) σ)

vectors :: Signature -> [Tag] -> Vector
vectors σ = foldl (⊕) (vector0 σ) . map (vector σ Exactly)

countsOfVector :: Vector -> [Counter]
countsOfVector (Vector cs) = cs

get :: Signature -> Vector -> Tag -> Counter
get σ (Vector cs) tag = aux σ cs
  where
    aux ((tag', _) : _) (c : _) | tag == tag' = c
    aux (_ : σ) (_ : cs) = aux σ cs

bounds :: Signature -> Type -> Vector
bounds σ = aux . simplify
  where
    aux Zero = vector0 σ
    aux One = vector0 σ
    aux (Tag tag) = vector σ Exactly tag
    aux (Or t s) = aux t ⊔ aux s
    aux (And t s) = aux t ⊕ aux s
    aux (Then t s) = aux t ⊕ aux s
    aux (Star t) = star (aux t)

-------------------------------------
-- RAPPRESENTAZIONE DELLE REAZIONI --
-------------------------------------

type Reaction = Vector

fire :: Vector -> Reaction -> Bool
fire v r = not (null (v ⊖ r))

fires :: Vector -> [Reaction] -> Bool
fires v = any (fire v)

messagesOfReaction :: [[Tag]] -> Signature -> Reaction -> [Tag]
messagesOfReaction mss σ (Vector cs) = ms
  where
    [ms] = filter (\ms -> sort ms == sort sms) mss
    sms = map (fst . fst) (filter ((/= Exactly 0) . snd) (zip σ cs))

reactions :: Vector -> [Reaction] -> [Reaction]
reactions v = filter (fire v)

-----------------
-- TRANSITIONS --
-----------------

data Label
  = Receive Tag
  | Consume Vector
  | Fire Tag Vector
  deriving (Eq, Ord)

instance Show Label where
  show (Receive tag) = "label=\"" ++ tag ++ "\""
  show (Consume v) = "label=\"" ++ show v ++ "\",style=dashed"
  show (Fire tag v) = "label=\"" ++ tag ++ "\",style=dotted"

showLabelPaper :: Label -> String
showLabelPaper (Consume v) = "label=\"" ++ showVectorPaper v ++ "\",style=dashed"
showLabelPaper l = show l

type Automaton = A.Automaton Vector Label
type Transition = A.Transition Vector Label

data State = State Vector [Maybe State]

allStates :: Signature -> Type -> Maybe State
allStates σ = auxM (vector0 σ)
  where
    auxM :: Vector -> Type -> Maybe State
    auxM _ t | zero t = Nothing
    auxM v t = Just (auxS v t)

    auxS :: Vector -> Type -> State
    auxS v t = State v (map (auxT v t) σ)

    auxT :: Vector -> Type -> (Tag, MessageType) -> Maybe State
    auxT v t (tag, _) | s ≈ s' = auxM (v ⊕ vector σ AtLeast tag) s
                      | otherwise = auxM (v ⊕ vector σ Exactly tag) s
      where
        s = approxDerive t tag
        s' = approxDerive s tag

allTransitions :: Signature -> [Reaction] -> Maybe State -> Automaton
allTransitions σ rs = A.make . auxS
  where
    auxS :: Maybe State -> [Transition]
    auxS Nothing = []
    auxS (Just (State v mss)) = auxN v mss

    auxN :: Vector -> [Maybe State] -> [Transition]
    auxN v mss = [ A.Transition v (Receive tag) w | ((tag, _), Just (State w _)) <- zip σ mss ]
                 ++ [ A.Transition v (Consume w) v' | w <- rs, v' <- v ⊖ w ]
                 ++ concat [ auxN w mss' | Just (State w mss') <- mss, v /= w ]

raceFreeState :: Automaton -> Vector -> Bool
raceFreeState a v = not $ any (`elem` ls') ls
  where
    vs = A.from a v (const True)
    ls = A.labelsFrom a v
    ls' = concat $ map (A.labelsFrom a) vs

automaton :: Signature -> [[Tag]] -> Type -> Automaton
automaton σ mss t = a
  where
    rs = map (vectors σ) mss
    a = allTransitions σ rs (allStates σ t)
