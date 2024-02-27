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

module Type where

import qualified Data.Set as S

data Type = Zero
          | One
          | Tag String
          | Or Type Type
          | And Type Type
          | Then Type Type
          | Star Type
          deriving (Eq, Ord, Show)

one :: Type -> Bool
one Zero = False
one One = True
one (Tag _) = False
one (Or t s) = one t || one s
one (And t s) = one t && one s
one (Then t s) = one t && one s
one (Star _) = True

zero :: Type -> Bool
zero t = simplify t == Zero

derive :: Type -> String -> Type
derive t tag = aux t
  where
    aux (Tag tag') | tag == tag' = One
    aux (Or t s) = Or (aux t) (aux s)
    aux (And t s) = Or (And (aux t) s) (And t (aux s))
    aux (Then t s) | one t = Or (Then (aux t) s) (aux s)
                   | otherwise = Then (aux t) s
    aux (Star t) = Then (aux t) (Star t)
    aux _ = Zero

approxDerive = derive

-- approxDerive :: Type -> String -> Type
-- approxDerive t tag = simplify (aux (simplify t))
--   where
--     aux (Tag tag') | tag == tag' = One
--     aux (t :|: s) = aux t :|: aux s
--     aux (t :&: s) = (aux t :&: s) :|: (t :&: aux s)
--     aux t@(Star s) | tag `S.member` (signature s) = t
--     aux _ = Zero

signature :: Type -> S.Set String
signature = aux . simplify
  where
    aux Zero = S.empty
    aux One = S.empty
    aux (Tag tag) = S.singleton tag
    aux (Or t s) = aux t `S.union` aux s
    aux (And t s) = aux t `S.union` aux s
    aux (Then t s) = aux t `S.union` aux s
    aux (Star t) = aux t

(≈) :: Type -> Type -> Bool
(≈) t₁ t₂ = sem t₁ == sem t₂
  where
    sem :: Type -> S.Set Type
    sem Zero = S.empty
    sem (Or t s) = sem t `S.union` sem s
    sem (And t s) = S.fromList [ And t' s'
                               | t' <- S.toList (sem t),
                                 s' <- S.toList (sem s) ]
    sem (Then t s) = S.fromList [ Then t' s'
                                | t' <- S.toList (sem t),
                                  s' <- S.toList (sem s) ]
    sem t = S.singleton t

simplify :: Type -> Type
simplify (Or t s) =
  case (simplify t, simplify s) of
    (Zero, s) -> s
    (t, Zero) -> t
    (t, s)    -> Or t s
simplify (And t s) =
  case (simplify t, simplify s) of
    (Zero, s) -> Zero
    (t, Zero) -> Zero
    (One, s)  -> s
    (t, One)  -> t
    (t, s)    -> And t s
simplify (Then t s) =
  case (simplify t, simplify s) of
    (Zero, s) -> Zero
    (t, Zero) -> Zero
    (One, s)  -> s
    (t, One)  -> t
    (t, s)    -> Then t s
simplify (Star t) =
  case simplify t of
    Zero -> One
    One -> One
    t -> Star t
simplify t = t
