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
    -- *** Ascii name
    nameGenA,
    varIDAGen,varGenA, conIDAGen, conGenA,
    isVarIDA, isConIDA,

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

    poisonGen,
) where

-----------------------------------------------------------------------------

import Control.Applicative

import Data.Char(isUpper, isLower, isDigit, isAscii, chr)
import Data.List(intercalate, findIndex)
import Data.Maybe(fromMaybe)
import Data.Traversable(mapAccumL)

import Test.QuickCheck
import Language.Haskell.Exts.Syntax

-----------------------------------------------------------------------------

infixr 9 .&&
infixr 8 .||

(.&&), (.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = \v -> p1 v && p2 v
p1 .|| p2 = \v -> p1 v || p2 v

-----------------------------------------------------------------------------


-- | Generates integers which are semi-poison distuted, that is the poison
-- distribution PMF is cut of at c1. The remainder of the PMF is constant
-- for the numbers c1 .. c2. For all other values it is 0. To approximate
-- a poison distribution c1 should be quite a bit larger than lambda.
poisonGen
    :: Double -- ^ Poison parameter (lambda)
    -> Int    -- ^ Poison cutoff c1
    -> Int    -- ^ Linear cutoff c2, c2 > c1
    -> Gen Int
poisonGen lambda c1 c2 | c1 >= c2 || lambda <= 0 = error $
    "poisonGen: invalid parameters " ++ show lambda ++ ", " ++ show c1 ++ ", " ++ show c2
poisonGen lambda c1 c2 =
    let (tot,cmf) = mapAccumL (\acc v -> (acc + v, acc + v)) 0 $ poisonPMFpart
    in choose (0, 1) >>= \x ->
        return $ if x < tot
                  then fromMaybe c1 $ findIndex (x <=) cmf
                  else let frac = (x - tot) / (1 - tot) -- fraction in the range c1 -> c2
                           part = frac * (fromIntegral $ c2 - c1) -- offset from c1
                       in c1 + ceiling part

    where
        poisonPMFpart = map (\k -> (exp (-lambda)) * (lambda ^ k) / (fromIntegral $ fact k)) [0..c1]
        fact :: Int -> Integer
        fact 0 = 1
        fact n = product [1 .. (fromIntegral n)]

plistOf1 :: Gen a -> Gen [a]
plistOf1 g = (:) <$> g <*> (poisonGen 4 10 20 >>= \i -> vectorOf i g)

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
        parts = listOf1 (nameStringGen `suchThat` isModIDPart)

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
isIDPart = isLower .|| isUpper .|| isDigit .|| (== '\'') .|| (== '_')

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

nameGenA :: Gen Name
nameGenA = withNameDistA (4, 5, 1, 3) id

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

withNameDistA :: NameDistribution -> (Name -> a) -> Gen a
withNameDistA nd g = withNameDistA' nd g g
withNameDistA' :: NameDistribution -> (Name -> a) -> (Name -> a) -> Gen a
withNameDistA' (ci, vi, cs, vs) gc gv
    = frequency
        [ (ci, gc <$> conIDAGen)
        , (vi, gv <$> varIDAGen)
        , (cs, gc <$> conSymGen)
        , (vs, gv <$> varSymGen)]


varIDGen, varSymGen, varGen, conIDGen, conSymGen, conGen :: Gen Name
varIDAGen, varGenA, conIDAGen, conGenA :: Gen Name
varIDGen    = Ident  <$> suchThat nameStringGen isVarID'
varIDAGen   = Ident  <$> suchThat nameStringAGen isVarIDA'
varSymGen   = Symbol <$> suchThat symGen isVarSym'
varGen      = frequency [(5, varIDGen), (3, varSymGen)]
varGenA     = frequency [(5, varIDAGen), (3, varSymGen)]
conIDGen    = Ident  <$> suchThat nameStringGen isConID'
conIDAGen   = Ident  <$> suchThat nameStringAGen isConIDA'
conSymGen   = Symbol <$> suchThat ((':':) <$> symGen) isConSym'
conGen      = frequency [(4, conIDGen), (1, conSymGen)]
conGenA     = frequency [(4, conIDAGen), (1, conSymGen)]

symGen :: Gen String
symGen = listOf1 $ elements asciiSyms
nameStringAGen :: Gen String
nameStringAGen = plistOf1 . elements. filter isIDPart . map chr $ [0..126]
nameStringGen :: Gen String
nameStringGen = plistOf1 (arbitrary `suchThat` isIDPart)

isVar, isCon, isConID, isConIDA, isVarID, isVarIDA, isConSym, isVarSym :: Name -> Bool
isCon = isConID .|| isConSym
isVar = isVarID .|| isVarSym

isConID  = isIdent isConID'
isConIDA = isIdent isConIDA'
isVarID  = isIdent isVarID'
isVarIDA = isIdent  isVarIDA'
isConSym = isSymbol isConSym'
isVarSym = isSymbol isVarSym'

isIdent :: (String -> Bool) -> Name -> Bool
isIdent p (Ident  n) = p n
isIdent _ (Symbol _) = False

isSymbol :: (String -> Bool) -> Name -> Bool
isSymbol _ (Ident  _) = False
isSymbol p (Symbol s) = p s

isConID', isConIDA', isVarID', isVarIDA', isConSym', isVarSym' :: String -> Bool
isConID'  = isConIDG (const True)
isConIDA' = isConIDG isAscii
isVarID'  = isVarIDG (const True)
isVarIDA' = isVarIDG isAscii
isConSym' s = isID (==':') isAsciiSym s
    && not (s `elem` reservedop)
isVarSym' s = isID (isAsciiSym .&&  (/= ':')) isAsciiSym s
    && not (s `elem` reservedop)
    && not (all (=='-') s)

isConIDG, isVarIDG :: (Char -> Bool) -> String -> Bool
isConIDG f = all f .&& isID isUpper isIDPart
isVarIDG f ns = all f ns
    && isID (isLower .|| (== '_')) isIDPart ns
    && not (ns `elem` reservedid)

isAsciiSym :: Char -> Bool
isAsciiSym c = c `elem` asciiSyms

asciiSyms :: String
asciiSyms = "!#$%&*+./<=>?@\\^|-~:"

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
