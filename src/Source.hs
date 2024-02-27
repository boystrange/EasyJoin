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

module Source where

import qualified Language.Java.Syntax as Java

type Tag = String
data MessageType = Asyn [Java.Modifier] [Java.FormalParam]
                 | Esyn [Java.Modifier] (Maybe Java.Type)
                 | Sync [Java.Modifier] (Maybe Java.Type) [Java.FormalParam]
  deriving Eq

data Annotation = NoAnnotation
                | Synchronous
                | Asynchronous
                | Esynchronous
                | Chord
                | Protocol String

type Signature = [(Tag, MessageType)]

asynchronous :: Signature -> Tag -> Bool
asynchronous σ = not . synchronous σ

synchronous :: Signature -> Tag -> Bool
synchronous σ tag =
  case lookup tag σ of
    Just (Sync _ _ _) -> True
    _ -> False

