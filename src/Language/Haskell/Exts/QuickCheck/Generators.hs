-----------------------------------------------------------------------------
--
-- Module      :  Language.Haskell.Exts.QuickCheck.Generators
-- Copyright   :  (c) 2012 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Defines generators and shrinkers for haskell-src-exts elements.
--
-----------------------------------------------------------------------------

module Language.Haskell.Exts.QuickCheck.Generators (
    -- * Modules
    -- ** WarningText
    warningTextGen, shrinkWarningText,

    -- ** Assoc
    assocGen, assocLRGen,

    -- * Types
    -- ** Boxed
    boxedGen, shrinkBoxed,

    -- * Literals
    -- ** Literal
    literalGen,
    nonPrimLit, primLit,

    -- * Variables, Constructors and Operators
    -- ** ModuleName
    moduleNameGen, shrinkModuleName,

    -- ** Name helpers
    NameDistribution,
    -- ** QName
    qnameGen,

    -- ** Name
    nameGen,
    varIDGen, varSymGen, varGen, conIDGen, conSymGen, conGen,
    -- *** Name propositions
    isVarID, isVarSym, isVar, isConID, isConSym, isCon,

    -- ** QOp
    qopGen,
    -- ** Op
    opGen, opGenWithDist,
    -- ** SpecialCon
    specialConGen,
    specialConGen',
    -- ** CName
    cnameGen, cnameGenWithDist,

    -- * FFI
    -- ** Safety
    safetyGen, shrinkSafety,
    -- ** CallConv
    callConvGen,

    -- * Pragmas
    -- ** Tool
    toolGen,
) where

-----------------------------------------------------------------------------

import Control.Applicative

import Data.Char(isUpper, isLower, isAlphaNum)
import Data.List(intercalate)

import Test.QuickCheck
import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------

infixr 9 .&&
infixr 8 .||

(.&&), (.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = \v -> p1 v && p2 v
p1 .|| p2 = \v -> p1 v || p2 v

-----------------------------------------------------------------------------
-- Module

-----------------------------------------------------------------------------
-- WarningText

warningTextGen :: Gen WarningText
warningTextGen = oneof $ [WarnText <$> arbitrary, DeprText <$> arbitrary]

shrinkWarningText :: WarningText -> [WarningText]
shrinkWarningText (DeprText t) = map DeprText $ shrink t
shrinkWarningText (WarnText t) = map WarnText $ shrink t

-----------------------------------------------------------------------------
-- ExportSpec
-- ImportDecl
-- ImportSpec

-----------------------------------------------------------------------------
-- Assoc

assocGen :: Gen Assoc
assocGen = elements [AssocNone, AssocLeft, AssocRight]

assocLRGen :: Gen Assoc
assocLRGen = elements [AssocLeft, AssocRight]

-----------------------------------------------------------------------------
-- Decl
-- Binds
-- IPBind
-- ClassDecl
-- InstDecl
-- Deriving (Maybe this could be arbitrary?)
-- DataOrNew (Cannot be arbitrary)
-- ConDecl
-- QualConDecl
-- GadtDecl
-- BangType
-- Match
-- Rhs
-- GuardedRhs

-- Context
-- FunDep
-- Asst
-- Type
-----------------------------------------------------------------------------
-- Boxed

boxedGen :: Gen Boxed
boxedGen =  elements [Unboxed, Boxed, Boxed]

shrinkBoxed :: Boxed -> [Boxed]
shrinkBoxed Unboxed = [Boxed]
shrinkBoxed Boxed   = []

-----------------------------------------------------------------------------
-- Kind
-- TyVarBind

-- Exp
-- Stmt
-- QualStmt
-- FieldUpdate
-- Alt
-- GuardedAlts
-- GuardedAlt
-- XAttr

-- Pat
-- PatField
-- PXAttr
-- RPat
-- RPatOp

-----------------------------------------------------------------------------
-- Literal

literalGen :: Gen Literal
literalGen  = oneof $
    [ Char          <$> arbitrary
    , String        <$> arbitrary
    , Int           <$> arbitrary
    , Frac          <$> arbitrary
    , PrimInt       <$> arbitrary
    , PrimWord      <$> arbitrary
    , PrimFloat     <$> arbitrary
    , PrimDouble    <$> arbitrary
    , PrimChar      <$> arbitrary
    , PrimString    <$> arbitrary
    ]

isPrim :: Literal -> Bool
isPrim (Char   _) = False
isPrim (String _) = False
isPrim (Int    _) = False
isPrim (Frac   _) = False
isPrim _          = True

nonPrimLit, primLit :: Gen Literal
nonPrimLit = literalGen `suchThat` (not . isPrim)
primLit    = literalGen `suchThat` isPrim

-----------------------------------------------------------------------------

moduleNameGen :: Gen ModuleName
moduleNameGen = fmap (ModuleName . intercalate ".") parts
    where
        parts :: Gen [String]
        parts = listOf1 (arbitrary `suchThat` isModIDPart)

shrinkModuleName :: ModuleName -> [ModuleName]
shrinkModuleName (ModuleName n) = map ModuleName $ shrink' n
    where
        shrink' :: String -> [String]
        shrink' n' = case break (== '.') n' of
            ([], _)      -> []
            (_ , '.':ns) -> ns : shrink' ns
            (ns, [])     -> [ns]
            (_, _)       -> error $ "shrink: unexpected character in:" ++ n

isModIDPart :: String -> Bool
isModIDPart = isID isUpper isIDPart

isID :: (Char -> Bool) -> (Char -> Bool) -> String -> Bool
isID _ _ [] = False
isID f g (n:ns) = f n && all g ns

isIDPart :: Char -> Bool
isIDPart = isAlphaNum .|| (== '\'')

-----------------------------------------------------------------------------
-- QName

qnameGen :: Gen Name -> Gen ModuleName -> Gen SpecialCon -> Gen QName
qnameGen ng mg sg = frequency
    [ (5, UnQual  <$> ng)
    , (2, Qual    <$> mg <*> ng)
    , (1, Special <$> sg)
    ]

-----------------------------------------------------------------------------
-- Name
nameGen :: Gen Name
nameGen  = withNameDist (4, 5, 1, 3) id

type NameDistribution = (Int, Int, Int, Int)

withNameDist :: NameDistribution -> (Name -> a) -> Gen a
withNameDist nd g = withNameDist' nd g g
withNameDist' :: NameDistribution -> (Name -> a) -> (Name -> a) -> Gen a
withNameDist' (ci, vi, cs, vs) gc gv
    = frequency
        [ (ci, gc <$> conIDGen)
        , (vi, gv <$> varIDGen)
        , (cs, gc <$> conSymGen)
        , (vs, gv <$> varSymGen)]


varIDGen, varSymGen, varGen, conIDGen, conSymGen, conGen :: Gen Name
varIDGen    = Ident  <$> suchThat arbitrary isVarID'
varSymGen   = Symbol <$> suchThat arbitrary isVarSym'
varGen      = frequency [(4, varIDGen), (3, varSymGen)]
conIDGen    = Ident  <$> suchThat arbitrary isConID'
conSymGen   = Symbol <$> suchThat arbitrary isConSym'
conGen      = frequency [(4, conIDGen), (1, conSymGen)]


isVar, isCon, isConID, isVarID, isConSym, isVarSym :: Name -> Bool
isCon n = isConID n || isConSym n
isVar n = isVarID n || isVarSym n
isConID (Ident  n) = isConID' n
isConID (Symbol _) = False
isVarID (Ident  n) = isVarID' n
isVarID (Symbol _) = False
isConSym (Ident  _) = False
isConSym (Symbol s) = isConSym' s
isVarSym (Ident  _) = False
isVarSym (Symbol s) = isVarSym' s

isConID', isVarID', isConSym', isVarSym' :: String -> Bool
isConID' = isID isUpper isIDPart
isVarID' ns = isID isLower isIDPart ns
    && not (ns `elem` reservedid)
isConSym' s = isID (==':') isAsciiSym s
    && not (s `elem` reservedop)
isVarSym' s = isID (isAsciiSym .&&  (/= ':')) isAsciiSym s
    && not (s `elem` reservedop)
    && not (all (=='-') s)

isAsciiSym :: Char -> Bool
isAsciiSym c = c `elem` "!#$%&*+./<=>?@\\^|-~:"

reservedid :: [String]
reservedid =
    [ "case", "class", "data", "default", "deriving", "do", "else"
    , "foreign", "if", "import", "in", "infix", "infixl"
    , "infixr", "instance", "let", "module", "newtype", "of"
    , "then", "type", "where", "_"
    ]

reservedop :: [String]
reservedop = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

-----------------------------------------------------------------------------
-- QOp
qopGen :: Gen Op -> Gen ModuleName -> Gen SpecialCon -> Gen QOp
qopGen og mg sg = frequency
    [ (5, liftOp   UnQual  <$> og)
    , (2, liftOp . Qual    <$> mg <*> og)
    , (1, QConOp . Special <$> sg)
    ]
    where
        liftOp :: (Name -> QName) -> Op -> QOp
        liftOp f (VarOp n) = QVarOp $ f n
        liftOp f (ConOp n) = QConOp $ f n
-- Op
opGen :: Gen Op
opGen = opGenWithDist (2, 3, 5, 1)

opGenWithDist :: NameDistribution -> Gen Op
opGenWithDist nd = withNameDist' nd ConOp VarOp

-----------------------------------------------------------------------------
-- SpecialCon

specialConGen :: Gen SpecialCon
specialConGen = specialConGen' 5 2 7 boxedGen

specialConGen'
    :: Int -- ^ frequency of normal cons
    -> Int -- ^ frequency of tuple cons
    -> Int -- ^ maximum size of tuple cons
    -> Gen Boxed
    -> Gen SpecialCon
specialConGen' nf tf mts bg
    = frequency
        [ (nf, elements [UnitCon, ListCon, FunCon, Cons, UnboxedSingleCon])
        , (tf, TupleCon <$> bg <*> choose (2, mts))
        ]

-----------------------------------------------------------------------------
-- CName, as far as it's usefull

cnameGen :: Gen CName
cnameGen = cnameGenWithDist (5, 5, 1, 3)

cnameGenWithDist :: NameDistribution -> Gen CName
cnameGenWithDist nd = withNameDist' nd ConName VarName

-----------------------------------------------------------------------------

-- IPName
-- XName

-- Bracket
-- Splice

-----------------------------------------------------------------------------

safetyGen :: Gen Safety
safetyGen = elements [PlayRisky, PlaySafe False, PlaySafe True]

shrinkSafety :: Safety -> [Safety]
shrinkSafety (PlaySafe True) = [PlaySafe False]
shrinkSafety _               = []

callConvGen :: Gen CallConv
callConvGen = elements [StdCall, CCall]

-----------------------------------------------------------------------------

-- ModulePragma
-- Tool

toolGen
    :: Bool -- ^ Allow the unknown tool
    ->  Gen Tool
toolGen unknown = frequency
    [ (10, elements [GHC, HUGS, NHC98, YHC, HADDOCK])
    , (uf, UnknownTool <$> suchThat arbitrary (all isIDPart) )]
    where uf = if unknown then 1 else 0
-- Rule
-- RuleVar
-- Activation
-- Annotation

-----------------------------------------------------------------------------
