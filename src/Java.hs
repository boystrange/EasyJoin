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

module Java (java) where

import Debug.Trace (traceShow, traceShowId)

import Control.Monad (forM_, when, unless)
import Data.Maybe (fromMaybe)
import Data.List (intercalate, nub, transpose, elemIndex, (\\))

import qualified Language.Java.Syntax as Java
import Source
import Type
import Compiler
import qualified Automaton as A

--------------------
-- JAVA GENERATOR --
--------------------

data QueueKind = None
               | Counter
               | Future (Maybe Java.Type)
               | Buffer Java.Type
               | Queue Java.Type

makeClassType :: String -> [Java.RefType] -> Java.Type
makeClassType name types = Java.RefType $ Java.ClassRefType $ Java.ClassType [(Java.Ident name, map Java.ActualType types)]

box :: Java.Type -> Java.Type
box (Java.PrimType primType) | Just name <- lookup primType boxMap = makeClassType name []
  where
    boxMap = [ (Java.BooleanT, "Boolean")
             , (Java.IntT,     "Integer") ]
box typ = typ

java :: Bool -> Signature -> [[Tag]] -> Type -> Automaton -> [Java.MemberDecl]
java interruptible σ mss t a = classMembers
  where
    tags = map fst σ
    bv = bounds σ t
    rs = map (vectors σ) mss

    stateMap = zip (A.states a) [0..]

    waitList :: Vector -> [Tag]
    waitList v = concat $ map (filter (synchronous σ)) $ map (messagesOfReaction mss σ) (reactions v rs)

    affine :: Tag -> Bool
    affine tag =
      case get σ bv tag of
        Exactly 1 -> True
        AtLeast _ -> False

    messageType :: Tag -> MessageType
    messageType tag | Just mt <- lookup tag σ = mt

    queueKind :: Tag -> QueueKind
    queueKind tag =
      case (affine tag, messageType tag) of
        (False, Asyn _ []) -> Counter
        (False, Asyn _ [Java.FormalParam _ typ _ _]) -> Queue (box typ)
        (True,  Asyn _ []) -> None
        (True,  Asyn _ [Java.FormalParam _ typ _ _]) -> Buffer (box typ)
        (False, Sync _ _ _) -> Counter
        (True,  Sync _ _ _) -> None
        (False, Esyn _ typeOpt) -> Queue (asynchronousReplyType (fmap box typeOpt))
        (True,  Esyn _ typeOpt) -> Buffer (asynchronousReplyType (fmap box typeOpt))

    -- messageClass :: Tag -> [Java.FormalParam] -> Java.ClassDecl
    -- messageClass tag params = Java.ClassDecl modifiers ident [] Nothing [] body
    --   where
    --     modifiers = [Java.Private, Java.Static]
    --     ident = Java.Ident (generatorPrefix ++ "message_" ++ tag)
    --     body = Java.ClassBody (paramMembers ++ [constructor])

    --     paramMembers :: [Java.Decl]
    --     paramMembers = map paramMember params

    --     paramMember :: Java.FormalParam -> Java.Decl
    --     paramMember (Java.FormalParam _ typ _ id) = Java.MemberDecl $ Java.FieldDecl [Java.Public] typ [Java.VarDecl id]

    --     constructor :: Java.Decl
    --     constructor = Java.MemberDecl $ Java.ConstructorDecl [Java.Public] [] ident params [] Nothing constructorBody

    --     constructorBody :: Java.ConstructorBody
    --     constructorBody = Java.ConstructorBody Nothing constructorCopyAll

    --     constructorCopyAll :: [Java.BlockStmt]
    --     constructorCopyAll = map constructorCopy params

    --     constructorCopy :: Java.FormalParam -> Java.BlockStmt
    --     constructorCopy (Java.FormalParam _ typ _ id) = Java.BlockStmt $ Java.ExpStmt $ Java.Assign (Java.FieldLhs $ Java.PrimaryFieldAccess Java.This id) Java.EqualA (Java.ExpName $ Java.Ident id)

    classMembers :: [Java.MemberDecl]
    classMembers = [lockMember] ++ condMembers ++ [stateMember] ++ queueMembers ++ generateMethods

    generatorPrefix :: String
    generatorPrefix = "ej_"

    lockMember :: Java.MemberDecl
    lockMember = Java.FieldDecl modifiers javaType [Java.VarDecl (Java.VarId lockIdent) (Just init)]
      where
        modifiers = [Java.Private, Java.Final]
        javaType = Java.RefType $ Java.ClassRefType $ reentrantLockClassType
        reentrantLockClassType = Java.ClassType [(reentrantLockIdent, [])]
        init = Java.InitExp $ Java.InstanceCreation [] (Java.TypeDeclSpecifier reentrantLockClassType) [] Nothing

    condMembers :: [Java.MemberDecl]
    condMembers = map condMember $ filter (synchronous σ) $ map fst σ

    condMember :: Tag -> Java.MemberDecl
    condMember tag = Java.FieldDecl modifiers javaType [Java.VarDecl (Java.VarId (changedIdent tag)) (Just init)]
      where
        modifiers = [Java.Private, Java.Final]
        javaType = Java.RefType $ Java.ClassRefType $ conditionClassType
        conditionClassType = Java.ClassType [(conditionIdent, [])]
        init = Java.InitExp $ invokeExp lockExp "newCondition" []

    stateMember :: Java.MemberDecl
    stateMember = Java.FieldDecl modifiers javaType [Java.VarDecl (Java.VarId stateIdent) (Just init)]
      where
        modifiers = [Java.Private]
        javaType = Java.PrimType Java.IntT
        init = Java.InitExp $ stateOfVectorExp $ vector0 σ

    intOfVector :: Vector -> Int
    intOfVector v | Just n <- lookup v stateMap = n

    stateOfVectorExp :: Vector -> Java.Exp
    stateOfVectorExp v = intExp $ intOfVector v

    messageArguments :: Tag -> [Java.FormalParam]
    messageArguments tag =
      case lookup tag σ of
        Just (Asyn _ formalParams) -> formalParams
        Just (Sync _ _ formalParams) -> formalParams
        Just (Esyn _ typeOpt) -> [Java.FormalParam [] (asynchronousReplyType typeOpt) False (Java.VarId $ Java.Ident tag)]

    identOfMessageArguments :: Tag -> [Java.Ident]
    identOfMessageArguments = map aux . messageArguments
      where
        aux (Java.FormalParam _ _ _ (Java.VarId ident)) = ident

    messageArgumentIdent :: Tag -> Java.Ident
    messageArgumentIdent = head . identOfMessageArguments

    messageArgumentExp :: Tag -> Java.Exp
    messageArgumentExp tag = Java.ExpName (Java.Name [messageArgumentIdent tag])

    voidType :: Java.Type
    voidType = Java.RefType $ Java.ClassRefType $ voidClassType

    voidClassType :: Java.ClassType
    voidClassType = Java.ClassType [(Java.Ident "Void", [])]

    asynchronousReplyType :: Maybe Java.Type -> Java.Type
    asynchronousReplyType = Java.RefType . Java.ClassRefType . asynchronousReplyClassType . fromMaybe voidType

    asynchronousReplyClassType :: Java.Type -> Java.ClassType
    asynchronousReplyClassType (Java.RefType typ) = Java.ClassType [(asynchronousReplyIdent, [Java.ActualType typ])]

    asynchronousReplyIdent :: Java.Ident
    asynchronousReplyIdent = Java.Ident "AsynchronousReply"

    asynchronousReplyInitExp :: Java.Exp
    asynchronousReplyInitExp = Java.InstanceCreation [] (Java.TypeDeclSpecifierUnqualifiedWithDiamond asynchronousReplyIdent Java.Diamond) [] Nothing

    linkedListType :: Java.Type -> Java.Type
    linkedListType = Java.RefType . Java.ClassRefType . linkedListClassType

    linkedListClassType :: Java.Type -> Java.ClassType
    linkedListClassType (Java.RefType javaType) = Java.ClassType [(linkedListIdent, [Java.ActualType javaType])]

    linkedListIdent :: Java.Ident
    linkedListIdent = Java.Ident "LinkedList"

    reentrantLockIdent :: Java.Ident
    reentrantLockIdent = Java.Ident "java.util.concurrent.locks.ReentrantLock"

    conditionIdent :: Java.Ident
    conditionIdent = Java.Ident "java.util.concurrent.locks.Condition"

    linkedListInitExp :: Java.Exp
    linkedListInitExp = Java.InstanceCreation [] (Java.TypeDeclSpecifierUnqualifiedWithDiamond linkedListIdent Java.Diamond) [] Nothing

    queueIdent :: Tag -> Java.Ident
    queueIdent tag = Java.Ident $ generatorPrefix ++ "queue_" ++ tag

    queueFieldAccess :: Tag -> Java.FieldAccess
    queueFieldAccess tag = Java.PrimaryFieldAccess Java.This (queueIdent tag)

    queueExp :: Tag -> Java.Exp
    queueExp = Java.FieldAccess . queueFieldAccess

    queueType :: Tag -> Java.Type
    queueType tag =
      case queueKind tag of
        Queue typ -> linkedListType typ
        Counter -> Java.PrimType Java.IntT
        Buffer typ -> typ

    initQueueExp :: Tag -> Java.Exp
    initQueueExp tag =
      case queueKind tag of
        Queue _ -> linkedListInitExp
        Counter -> intExp 0
        Buffer _ -> Java.Lit Java.Null

    nameOfReaction :: [Tag] -> String
    nameOfReaction tags = "when_" ++ intercalate "_" tags

    queueMembers :: [Java.MemberDecl]
    queueMembers = concat (map queueMember (map fst σ))

    intType :: Java.Type
    intType = Java.PrimType Java.IntT

    fieldDecl :: Java.Type -> Java.Ident -> Java.Exp -> Java.MemberDecl
    fieldDecl typ ident init = Java.FieldDecl [Java.Private] typ [Java.VarDecl (Java.VarId ident) (Just $ Java.InitExp init)]

    queueMember :: Tag -> [Java.MemberDecl]
    queueMember tag =
      case queueKind tag of
        None -> []
        Counter -> [fieldDecl intType (queueIdent tag) (intExp 0)]
        Buffer typ -> [fieldDecl typ (queueIdent tag) (Java.Lit Java.Null)]
        Queue typ -> [fieldDecl (linkedListType typ) (queueIdent tag) linkedListInitExp]

    generateMethods :: [Java.MemberDecl]
    generateMethods = concat (map generateMethod (map fst σ))

    throwIllegalStateExceptionStmt :: Java.Stmt
    throwIllegalStateExceptionStmt = Java.Throw newIllegalStateExceptionExp

    throwIllegalStateExceptionBlockStmt :: Java.BlockStmt
    throwIllegalStateExceptionBlockStmt = Java.BlockStmt throwIllegalStateExceptionStmt

    newIllegalStateExceptionExp :: Java.Exp
    newIllegalStateExceptionExp = Java.InstanceCreation [] (Java.TypeDeclSpecifier $ Java.ClassType [(Java.Ident "IllegalStateException", [])]) [] Nothing

    stateToStringMethod :: Java.MemberDecl
    stateToStringMethod = Java.MethodDecl modifiers [] (Just javaType) ident [] [] Nothing methodBody
      where
        modifiers = [Java.Public]
        javaType = Java.RefType $ Java.ClassRefType $ Java.ClassType [(Java.Ident "String", [])]
        ident = Java.Ident "stateToString"
        methodBody = Java.MethodBody $ Just $ Java.Block [Java.BlockStmt switchStmt]

        switchStmt = Java.Switch stateExp (map switchCase (A.states a) ++ [defaultCase])

        switchCase :: Vector -> Java.SwitchBlock
        switchCase v = Java.SwitchBlock (Java.SwitchCase $ stateOfVectorExp v) [returnState v]

        returnState :: Vector -> Java.BlockStmt
        returnState v = Java.BlockStmt $ Java.Return $ Just $ Java.Lit $ Java.String (show v)

        defaultCase :: Java.SwitchBlock
        defaultCase = Java.SwitchBlock Java.Default [throwIllegalStateExceptionBlockStmt]

    returnType :: Tag -> Maybe Java.Type
    returnType tag =
      case lookup tag σ of
        Just (Asyn _ _) -> Nothing
        Just (Sync _ typeOpt _) -> typeOpt

    reactionArguments :: [Tag] -> [Java.Exp]
    reactionArguments = map aux . concat . map identOfMessageArguments
      where
        aux :: Java.Ident -> Java.Exp
        aux ident = Java.ExpName $ Java.Name [ident]

    returnTypeOfReaction :: [Tag] -> Maybe Java.Type
    returnTypeOfReaction tags = returnType tag
      where
        [tag] = filter (synchronous σ) tags

    breakBlockStmt :: [Java.BlockStmt]
    breakBlockStmt = [Java.BlockStmt $ Java.Break Nothing]

    exc :: Java.ExceptionType
    exc = Java.ClassRefType $ Java.ClassType [(Java.Ident "InterruptedException", [])]

    exceptionsOf :: Tag -> [Java.ExceptionType]
    exceptionsOf tag | raceFreeMethod tag = []
                     | not interruptible = []
                     | otherwise = [exc]

    synchronousProxy :: [Java.Modifier] -> Maybe Java.Type -> Tag -> Java.MemberDecl
    synchronousProxy modifiers typeOpt tag = Java.MethodDecl modifiers [] typeOpt ident [] (exceptionsOf tag) Nothing methodBody
      where
        ident :: Java.Ident
        ident = Java.Ident tag

        methodBody :: Java.MethodBody
        methodBody = Java.MethodBody $ Just $ Java.Block [declareFuture, sendMessage, returnResult]

        declareFuture :: Java.BlockStmt
        declareFuture = defineLocalVar (asynchronousReplyType typeOpt) (Java.Ident "reply") asynchronousReplyInitExp

        sendMessage :: Java.BlockStmt
        sendMessage = blockStmtOfExp $ invokeExp Java.This tag [replyExp]

        replyExp :: Java.Exp
        replyExp = Java.ExpName $ Java.Name [Java.Ident "reply"]

        getMethod :: String
        getMethod | interruptible = "get"
                  | otherwise     = "getUninterruptibly"

        returnResult :: Java.BlockStmt
        returnResult =
          case typeOpt of
            Nothing -> blockStmtOfExp $ invokeExp replyExp getMethod []
            Just _  -> Java.BlockStmt $ Java.Return $ Just $ invokeExp replyExp getMethod []

    raceFreeMethod :: Tag -> Bool
    raceFreeMethod tag = all (raceFreeState a) (A.with a (withTag tag))

    lockBlockStmt :: Java.BlockStmt
    lockBlockStmt = blockStmtOfExp $ invokeExp lockExp "lock" []

    unlockBlockStmt :: Java.BlockStmt
    unlockBlockStmt = blockStmtOfExp $ invokeExp lockExp "unlock" []

    monitorExit :: Tag -> [Java.BlockStmt]
    monitorExit tag | raceFreeMethod tag = []
                    | otherwise = [unlockBlockStmt]

    monitorExitBeforeBreak :: Tag -> [Java.BlockStmt]
    monitorExitBeforeBreak tag | synchronous σ tag = []
                               | otherwise = monitorExit tag

    generateMethod :: Tag -> [Java.MemberDecl]
    generateMethod tag =
      case lookup tag σ of
        Just (Asyn modifiers formalParams) ->
          [ Java.MethodDecl modifiers [] Nothing ident formalParams [] Nothing methodBody ]
        Just (Esyn modifiers typeOpt) ->
          [ Java.MethodDecl modifiers [] Nothing ident (messageArguments tag) [] Nothing methodBody
          , synchronousProxy modifiers typeOpt tag ]
        Just (Sync modifiers typeOpt formalParams) ->
          [ Java.MethodDecl modifiers [] typeOpt ident formalParams (exceptionsOf tag) Nothing methodBody ]
      where
        adjust :: [Java.Modifier] -> [Java.Modifier]
        adjust modifiers | raceFreeMethod tag = modifiers
                         | otherwise = Java.Synchronized_ : modifiers

        monitorEnter :: [Java.BlockStmt]
        monitorEnter | raceFreeMethod tag = []
                     | otherwise = [lockBlockStmt]

        ident :: Java.Ident
        ident = Java.Ident tag

        methodBody :: Java.MethodBody
        methodBody = Java.MethodBody $ Just $ Java.Block (monitorEnter ++ insertInQueue tag ++ receiveBlockStmts ++ consumeBlockStmts)

        receiveBlockStmts :: [Java.BlockStmt]
        receiveBlockStmts = [Java.BlockStmt $ Java.Switch stateExp (receiveCases ++ receiveLoopCases ++ defaultReceiveCases)]

        -- all states from which there is a tag-labelled transition that is not a loop
        vs₁ :: [Vector]
        vs₁ = nub [ v | A.Transition v (Receive tag') w <- A.transitions a, v /= w, tag == tag' ]

        -- all states from which there is a firing tag-labelled loop
        vs₂ :: [Vector]
        vs₂ = nub [ v | A.Transition v (Receive tag') w <- A.transitions a, v == w, tag == tag' ]

        vs₃ :: [Vector]
        vs₃ = A.states a \\ (vs₁ ++ vs₂)

        receiveCases :: [Java.SwitchBlock]
        receiveCases = map (generateReceiveCase tag) (vs₁ \\ vs₂)

        receiveLoopCases :: [Java.SwitchBlock]
        receiveLoopCases =
          case reverse vs₂ of
            [] -> []
            (v : vs) ->
              reverse $ [Java.SwitchBlock (Java.SwitchCase $ stateOfVectorExp v) (monitorExitBeforeBreak tag ++ breakBlockStmt)] ++ [Java.SwitchBlock (Java.SwitchCase $ stateOfVectorExp w) [] | w <- vs]

        defaultReceiveCases :: [Java.SwitchBlock]
        defaultReceiveCases =
          [Java.SwitchBlock (Java.SwitchCase $ stateOfVectorExp w) [] | w <- vs₃]
          ++ [Java.SwitchBlock Java.Default $ monitorExit tag ++ [throwIllegalStateExceptionBlockStmt]]

        consumeBlockStmts :: [Java.BlockStmt]
        consumeBlockStmts | asynchronous σ tag = []
                          | raceFreeMethod tag = [Java.BlockStmt $ switchConsume]
                          | otherwise = [Java.BlockStmt $ Java.While (Java.Lit $ Java.Boolean True) switchConsume]

        switchConsume :: Java.Stmt
        switchConsume = Java.Switch stateExp (consumeCases ++ defaultConsumeCases)

        waitBlockStmt :: Tag -> Java.BlockStmt
        waitBlockStmt tag = blockStmtOfExp $ invokeExp (changedExp tag) "awaitUninterruptibly" []

        us₁ :: [Vector]
        us₁ = nub [ u | u <- A.states a, r <- rs, vector σ Exactly tag ⊑ r, r ⊑ u ]

        us₂ :: [Vector]
        us₂ = A.states a \\ us₁

        consumeCases :: [Java.SwitchBlock]
        consumeCases = map (generateConsumeCase tag) us₁

        defaultConsumeCases :: [Java.SwitchBlock]
        defaultConsumeCases =
          if raceFreeMethod tag
          then [Java.SwitchBlock Java.Default [throwIllegalStateExceptionBlockStmt]]
          else [Java.SwitchBlock (Java.SwitchCase $ stateOfVectorExp w) [] | w <- us₂ ]
               ++ [Java.SwitchBlock Java.Default [waitBlockStmt tag]]

    generateReceiveCase :: Tag -> Vector -> Java.SwitchBlock
    generateReceiveCase tag v = Java.SwitchBlock (Java.SwitchCase $ stateOfVectorExp v) (updateState ++ notifications ++ monitorExitBeforeBreak tag ++ breakBlockStmt)
      where
        updateState :: [Java.BlockStmt]
        updateState = [assignStateBlockStmt (stateOfVectorExp w)]

        notifications :: [Java.BlockStmt]
        notifications | raceFreeMethod tag = []
                      | synchronous σ tag = []
                      | otherwise = [ blockStmtOfExp $ invokeExp (changedExp tag') "signal" []
                                    | tag' <- waitList w ]

        w :: Vector
        [w] = A.from a v (withTag tag)

    generateConsumeCase :: Tag -> Vector -> Java.SwitchBlock
    generateConsumeCase tag v = Java.SwitchBlock (Java.SwitchCase $ stateOfVectorExp v) ([block] ++ returnOpt)
      where
        returnOpt :: [Java.BlockStmt]
        returnOpt = map Java.BlockStmt (if returnTypeOfReaction tags == Nothing then [Java.Return Nothing] else [])

        block :: Java.BlockStmt
        block = Java.BlockStmt stmt

        stmt :: Java.Stmt
        stmt = Java.StmtBlock (Java.Block stmts)

        stmts :: [Java.BlockStmt]
        stmts = concat (map removeFromQueue tags)
                ++ [ assignStateBlockStmt (computeState css) ]
                ++ monitorExit tag
                ++ [ Java.BlockStmt (fireReaction tags) ]

        -- gestire il caso di ambiguit\`a nelle reazioni
        r = head [ r | r <- rs, vector σ Exactly tag ⊑ r, r ⊑ v ]
        tags = messagesOfReaction mss σ r
        ws = v ⊖ vectors σ tags
        css = map nub (transpose (map countsOfVector ws))

    fireReaction :: [Tag] -> Java.Stmt
    fireReaction tags =
      case returnTypeOfReaction tags of
        Nothing -> Java.ExpStmt $ invokeExp Java.This (nameOfReaction tags) args
        Just _ -> Java.Return $ Just $ invokeExp Java.This (nameOfReaction tags) args
      where
        args = reactionArguments tags

    insertInQueue :: Tag -> [Java.BlockStmt]
    insertInQueue tag =
      case queueKind tag of
        None     -> [ ]
        Counter  -> [ incrementBlockStmt (queueExp tag) ]
        Buffer _ -> [ assignFieldBlockStmt (queueFieldAccess tag) (messageArgumentExp tag) ]
        Queue _  -> [ blockStmtOfExp $ invokeExp (queueExp tag) "add" [messageArgumentExp tag] ]

    removeFromQueue :: Tag -> [Java.BlockStmt]
    removeFromQueue tag =
      case queueKind tag of
        None       -> [ ]
        Counter    -> [ decrementBlockStmt (queueExp tag) ]
        Buffer typ -> [ defineLocalVar typ (messageArgumentIdent tag) (queueExp tag)
                      , assignFieldBlockStmt (queueFieldAccess tag) (Java.Lit Java.Null) ]
        Queue typ  -> [ defineLocalVar typ (messageArgumentIdent tag) (invokeExp (queueExp tag) "remove" []) ]

    defineLocalVar :: Java.Type -> Java.Ident -> Java.Exp -> Java.BlockStmt
    defineLocalVar javaType ident e = Java.LocalVars [Java.Final] javaType [Java.VarDecl (Java.VarId ident) (Just (Java.InitExp e))]

    computeState :: [[Counter]] -> Java.Exp
    computeState = aux [] σ
      where
        aux cs [] [] = stateOfVectorExp (Vector cs)
        aux cs (_ : σ) ([c] : css) = aux (cs ++ [c]) σ css
        aux cs ((tag, _) : σ) ([Exactly m, AtLeast n] : css)
          = condExp (eqExp (getQueueSize tag) (intExp m)) (aux (cs ++ [Exactly m]) σ css) (aux (cs ++ [AtLeast n]) σ css)

    decrementBlockStmt :: Java.Exp -> Java.BlockStmt
    decrementBlockStmt = blockStmtOfExp . Java.PostDecrement

    incrementBlockStmt :: Java.Exp -> Java.BlockStmt
    incrementBlockStmt = blockStmtOfExp . Java.PostIncrement

    blockStmtOfExp :: Java.Exp -> Java.BlockStmt
    blockStmtOfExp = Java.BlockStmt . Java.ExpStmt

    assignFieldExp :: Java.FieldAccess -> Java.Exp -> Java.Exp
    assignFieldExp field = Java.Assign (Java.FieldLhs field) Java.EqualA

    assignFieldBlockStmt :: Java.FieldAccess -> Java.Exp -> Java.BlockStmt
    assignFieldBlockStmt field = blockStmtOfExp . assignFieldExp field

    assignStateBlockStmt :: Java.Exp -> Java.BlockStmt
    assignStateBlockStmt = assignFieldBlockStmt stateField

    lockIdent :: Java.Ident
    lockIdent = Java.Ident $ generatorPrefix ++ "lock"

    changedIdent :: Tag -> Java.Ident
    changedIdent tag = Java.Ident $ generatorPrefix ++ "try_" ++ tag

    stateIdent :: Java.Ident
    stateIdent = Java.Ident $ generatorPrefix ++ "state"

    tableIdent :: Java.Ident
    tableIdent = Java.Ident $ generatorPrefix ++ "table"

    stateField :: Java.FieldAccess
    stateField = Java.PrimaryFieldAccess Java.This stateIdent

    lockField :: Java.FieldAccess
    lockField = Java.PrimaryFieldAccess Java.This lockIdent

    changedField :: Tag -> Java.FieldAccess
    changedField = Java.PrimaryFieldAccess Java.This . changedIdent

    stateExp :: Java.Exp
    stateExp = Java.FieldAccess stateField

    lockExp :: Java.Exp
    lockExp = Java.FieldAccess lockField

    changedExp :: Tag -> Java.Exp
    changedExp = Java.FieldAccess . changedField

    invokeExp :: Java.Exp -> String -> [Java.Exp] -> Java.Exp
    invokeExp e m es = Java.MethodInv $ Java.PrimaryMethodCall e [] (Java.Ident m) es

    intExp :: Int -> Java.Exp
    intExp = Java.Lit . Java.Int . fromIntegral

    eqExp :: Java.Exp -> Java.Exp -> Java.Exp
    eqExp e₁ e₂ = Java.BinOp e₁ Java.Equal e₂

    condExp :: Java.Exp -> Java.Exp -> Java.Exp -> Java.Exp
    condExp = Java.Cond

    getQueueSize :: Tag -> Java.Exp
    getQueueSize tag =
      case queueKind tag of
        Counter -> queueExp tag
        Queue _ -> Java.MethodInv (Java.PrimaryMethodCall (queueExp tag) [] (Java.Ident "size") [])

    withTag :: Tag -> Label -> Bool
    withTag tag (Receive tag') = tag == tag'
    withTag tag (Fire tag' _) = tag == tag'
    withTag _ _ = False
