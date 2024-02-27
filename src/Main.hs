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

module Main (main) where

import Debug.Trace (trace)

import qualified Language.Java.Syntax as Java
import Language.Java.Parser (compilationUnit, parser)
import Language.Java.Pretty (prettyPrint)
import Control.Monad (when)
import System.Console.GetOpt
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getProgName, getArgs)
import System.FilePath.Posix (takeBaseName, replaceExtensions)
import System.IO

import Aux
import Source
import Compiler
import Java
import Automaton (dot)
import qualified TypeParser

----------
-- MAIN --
----------

getAnnotation :: [Java.Modifier] -> (Annotation, [Java.Modifier])
getAnnotation [] = (NoAnnotation, [])
getAnnotation (Java.Annotation ann : modifiers) =
  case ann of
    Java.MarkerAnnotation (Java.Name [Java.Ident "Asynchronous"]) -> (Asynchronous, modifiers')
    Java.MarkerAnnotation (Java.Name [Java.Ident "State"]) -> (Asynchronous, modifiers')
    Java.MarkerAnnotation (Java.Name [Java.Ident "Esynchronous"]) -> (Esynchronous, modifiers')
    Java.MarkerAnnotation (Java.Name [Java.Ident "Synchronous"]) -> (Synchronous, modifiers')
    Java.MarkerAnnotation (Java.Name [Java.Ident "Operation"]) -> (Synchronous, modifiers')
    Java.MarkerAnnotation (Java.Name [Java.Ident "Chord"]) -> (Chord, modifiers')
    Java.MarkerAnnotation (Java.Name [Java.Ident "Reaction"]) -> (Chord, modifiers')
    Java.SingleElementAnnotation _ (Java.EVVal (Java.InitExp (Java.Lit (Java.String tsource)))) -> (Protocol tsource, modifiers')
    _ -> (ann', Java.Annotation ann : modifiers')
  where
    (ann', modifiers') = getAnnotation modifiers
getAnnotation (modifier : modifiers) = (ann, modifier : modifiers')
  where
    (ann, modifiers') = getAnnotation modifiers

processClassBody :: [Flag] -> String -> String -> Java.ClassBody -> IO Java.ClassBody
processClassBody flags className tsource (Java.ClassBody decls) = do
  let t = TypeParser.parseType tsource
  let (σ, mss, decls') = classifyDeclarations decls
  let a = Compiler.automaton σ mss t
  when (Dot `elem` flags) (dot showNode showLabel (nodeStyle a) className rankDir a)
  let decls'' = Java.java (Interruptible `elem` flags) σ mss t a
  return $ Java.ClassBody (map Java.MemberDecl decls'' ++ decls')
  where
    classifyDeclarations :: [Java.Decl] -> (Signature, [[Tag]], [Java.Decl])
    classifyDeclarations [] = ([], [], [])
    classifyDeclarations (decl@(Java.MemberDecl (Java.MethodDecl modifiers [] typeOpt (Java.Ident tag) formalParams excs Nothing (Java.MethodBody methodBodyOpt))) : decls) =
      case getAnnotation modifiers of
        (NoAnnotation, _) -> (σ, mss, decl : decls')
        (Asynchronous, modifiers) | nothing typeOpt && nothing methodBodyOpt -> ((tag, Asyn modifiers formalParams) : σ, mss, decls')
        (Esynchronous, modifiers) | null formalParams && nothing methodBodyOpt -> ((tag, Esyn modifiers typeOpt) : σ, mss, decls')
        (Synchronous, modifiers) | nothing methodBodyOpt -> ((tag, Sync modifiers typeOpt formalParams) : σ, mss, decls')
        (Chord, modifiers) -> (σ, extractAtoms tag : mss, (Java.MemberDecl (Java.MethodDecl modifiers [] typeOpt (Java.Ident tag) formalParams excs Nothing (Java.MethodBody methodBodyOpt))) : decls')
      where
        (σ, mss, decls') = classifyDeclarations decls
    classifyDeclarations (decl : decls) = (σ, mss, decl : decls')
      where
        (σ, mss, decls') = classifyDeclarations decls

    rankDir :: String
    rankDir = if LeftRight `elem` flags then "LR" else "TB"

    extractAtoms :: String -> [Tag]
    extractAtoms = tail . split '_'

    showNode :: Vector -> String
    showNode = if Paper `elem` flags then showVectorPaper else show

    showLabel :: Label -> String
    showLabel = if Paper `elem` flags then showLabelPaper else show

    nodeStyle a v | False && raceFreeState a v = "filled"
    nodeStyle _ _ = ""

processTypeDecl :: [Flag] -> Java.TypeDecl -> IO Java.TypeDecl
processTypeDecl flags (typeDecl@(Java.ClassTypeDecl (Java.ClassDecl modifiers (Java.Ident className) typeParams refTypeOpt refTypes classBody))) =
  case getAnnotation modifiers of
    (Protocol tsource, modifiers') -> do
       classBody' <- processClassBody flags className tsource classBody
       return $ Java.ClassTypeDecl (Java.ClassDecl modifiers' (Java.Ident className) typeParams refTypeOpt refTypes classBody')
    _ -> return typeDecl
processTypeDecl _ typeDecl = return typeDecl

main :: IO ()
main = do
  progName <- getProgName
  (args, inFile) <- getArgs >>= parse progName
  source <- readFile inFile
  case parser compilationUnit source of
    Left err -> error ("parse error: " ++ show err)
    Right (Java.CompilationUnit packageOpt importDecls typeDecls) -> do
      typeDecls' <- mapM (processTypeDecl args) typeDecls
      let importDecls' = Java.ImportDecl False (Java.Name [Java.Ident "java", Java.Ident "util", Java.Ident "LinkedList"]) False : importDecls
      let outFile = replaceExtensions inFile ".java"
      let target = prettyPrint $ Java.CompilationUnit packageOpt importDecls' typeDecls'
      when (Java `elem` args) (writeFile outFile target)

data Flag = Verbose       -- -v --verbose
          | LeftRight     --    --lr
          | Java          -- -j --java
          | Interruptible -- -i --interruptible
          | Dot           -- -d --dot
          | Help          --    --help
          | Paper         --    --paper
            deriving (Eq, Ord, Show)

flags :: [OptDescr Flag]
flags =
   [ Option "d" ["dot"]           (NoArg Dot)       "Generate DOT file"
   , Option "j" ["java"]          (NoArg Java)      "Generate Java file"
   , Option "i" ["interruptible"] (NoArg Interruptible) "Use interruptible await method"
   , Option ""  ["lr"]            (NoArg LeftRight) "Use LR rankdir for DOT"
   , Option "v" ["verbose"]       (NoArg Verbose)   "Print automaton statistics"
   , Option "h" ["help"]          (NoArg Help)      "Print this help message"
   , Option ""  ["paper"]         (NoArg Paper)     "Use paper notation in DOT file" ]

parse progName argv =
  case getOpt Permute flags argv of
    (args, files, []) -> do
      when (null files || length files > 1 || Help `elem` args)
        (do hPutStrLn stderr (usageInfo header flags)
            exitWith ExitSuccess)
      return (args, head files)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
  where
    header = "Usage: " ++ progName ++ " [options] [FILE]"
