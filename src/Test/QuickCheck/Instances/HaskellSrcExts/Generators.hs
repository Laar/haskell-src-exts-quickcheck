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

module Test.QuickCheck.Instances.HaskellSrcExts.Generators (
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
    CharGenMode(..),
    -- ** ModuleName
    moduleNameGen, shrinkModuleName,

    -- ** Name helpers
    -- ** QName
    QNameDist(..),
    qnameGen, qnameGenWithDist,

    -- ** Name
    NameDist(..),
    nameGen, nameGenWithDist,
    varIDGen, varSymGen, conIDGen, conSymGen,
    -- *** Name propositions
    isVarID, isVarSym, isVar, isConID, isConSym, isCon,

    -- ** QOp
    qopGen,
    qopGenWithDist,
    -- ** Op
    opGen, opGenWithDist,
    -- ** SpecialCon
    SpecialConDist(..),
    specialConGen,
    specialConGenWithDist,
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

    poisonGen, uniqueList,
) where

-----------------------------------------------------------------------------

import Control.Applicative

import Data.Char(isUpper, isLower, isDigit)
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

uniqueList :: Eq a => Int -> Gen a -> Gen [a]
uniqueList n g = go n (return [])
    where
        go 0 acc = acc
        go n' acc = do
            e <- g
            el <- elem e <$> acc
            if el
             then go n acc
             else go (n'-1) ((e:)<$>acc)

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
-- Defaul distributions

defaultNameDist, defaultOpNameDist :: NameDist
defaultNameDist   = (ND 4 5 1 3)
defaultOpNameDist = (ND 2 3 5 1)

defaultSpecialConDist :: SpecialConDist
defaultSpecialConDist = (SCD 5 2 [Boxed, Unboxed] 7)

defaultQNameDist, defaultQOpQNameDist :: QNameDist
defaultQNameDist    = (QND 5 2 1 defaultNameDist   defaultSpecialConDist)
defaultQOpQNameDist = (QND 5 2 1 defaultOpNameDist defaultSpecialConDist)

-----------------------------------------------------------------------------
-- ModuleName

moduleNameGen :: CharGenMode -> Gen ModuleName
moduleNameGen cgm = fmap (ModuleName . intercalate ".") parts
    where
        modNamePartGen :: Gen String
        modNamePartGen = (:) <$> charGenWith isUpper cgm <*> nameStringGen cgm
        parts :: Gen [String]
        parts = listOf1 modNamePartGen

shrinkModuleName :: ModuleName -> [ModuleName]
shrinkModuleName (ModuleName n) = map ModuleName $ shrink' n
    where
        shrink' :: String -> [String]
        shrink' n' = case break (== '.') n' of
            ([], _)      -> []
            (_ , '.':ns) -> ns : shrink' ns
            (ns, [])     -> [ns]
            (_, _)       -> error $ "shrink: unexpected character in:" ++ n

isID :: (Char -> Bool) -> (Char -> Bool) -> String -> Bool
isID _ _ [] = False
isID f g (n:ns) = f n && all g ns

isIDPart :: Char -> Bool
isIDPart = isLower .|| isUpper .|| isDigit .|| (== '\'') .|| (== '_')

-----------------------------------------------------------------------------
-- QName

qnameGen :: CharGenMode -> Gen QName
qnameGen = qnameGenWithDist defaultQNameDist

qnameGenWithDist :: QNameDist -> CharGenMode -> Gen QName
qnameGenWithDist qnd cgm =
    let genName = nameGenWithDist (nameDist qnd) cgm
    in frequency
        [ (unqualf  qnd, UnQual  <$> genName)
        , (qualf    qnd, Qual    <$> moduleNameGen cgm <*> genName)
        , (specialf qnd, Special <$> specialConGenWithDist (specialDist qnd))
        ]

data QNameDist
    = QND
    { unqualf       :: Int
    , qualf         :: Int
    , specialf      :: Int
    , nameDist      :: NameDist
    , specialDist   :: SpecialConDist
    }

-----------------------------------------------------------------------------
-- Name

data NameDist
    = ND
    { cidf  :: Int
    , vidf  :: Int
    , csymf :: Int
    , vsymf :: Int
    } deriving (Eq, Ord, Show)

data CharGenMode
    = Default
    | Ascii
    | SpecialMode (Char -> Bool) (Char -> Bool)

nameGen :: CharGenMode -> Gen Name
nameGen = nameGenWithDist defaultNameDist

nameGenWithDist :: NameDist -> CharGenMode -> Gen Name
nameGenWithDist = genFromNameWithDist id id

genFromNameWithDist :: (Name -> a) -> (Name -> a) -> NameDist -> CharGenMode -> Gen a
genFromNameWithDist gc gv ngd cm
    = frequency
        [ (cidf  ngd, gc <$> conIDGen  cm)
        , (vidf  ngd, gv <$> varIDGen  cm)
        , (csymf ngd, gc <$> conSymGen cm)
        , (vsymf ngd, gv <$> varSymGen cm)]

-----------------------------------------------------------------------------
-- the generators for the parts

charGen :: CharGenMode -> Gen Char
charGen Default           = arbitrary
charGen Ascii             = elements asciiChars
charGen (SpecialMode p _) = arbitrary `suchThat` p

symGen :: CharGenMode -> Gen Char
symGen Default           = arbitrary
symGen Ascii             = elements asciiSyms
symGen (SpecialMode _ p) = arbitrary `suchThat` p

charGenWith :: (Char -> Bool) -> CharGenMode -> Gen Char
charGenWith p m = charGen m `suchThat` p

nameStringGen :: CharGenMode -> Gen String
nameStringGen = plistOf1 . charGen

symGenWith :: (Char -> Bool) -> CharGenMode -> Gen Char
symGenWith  p m = symGen m `suchThat` p

symsGen :: CharGenMode -> Gen String
symsGen = plistOf1 . symGen


conIDGen, varIDGen, conSymGen, varSymGen :: CharGenMode -> Gen Name
conIDGen m = Ident <$> (charGenWith isUpper m <:> nameStringGen m)
varIDGen m = Ident <$> (charGenWith isSmall m <:> nameStringGen m) `notFrom` reservedid
    where isSmall = isLower .|| (=='_')
conSymGen m = Symbol <$> ((':':) <$> symsGen m)                `notFrom` reservedop
varSymGen m = Symbol <$> (symGenWith  (/=':') m <:> symsGen m) `notFrom` reservedop

(<:>) :: Applicative m => m a -> m [a] -> m [a]
a1 <:> a2 = (:) <$> a1 <*> a2
notFrom :: Eq a => Gen a -> [a] -> Gen a
notFrom g a = g `suchThat` (not . (`elem` a))

-----------------------------------------------------------------------------
-- listing of chars and reserved names

asciiChars, smallAChars, largeAChars, digits, asciiSyms :: String
asciiChars    = smallAChars ++ largeAChars ++ digits ++ "'"
smallAChars = ['a'..'z'] ++ "_"
largeAChars = ['A'..'Z']
digits      = ['0'..'9']
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
-- name checkers

isVar, isCon, isConID, isVarID, isConSym, isVarSym :: Name -> Bool
isCon = isConID .|| isConSym
isVar = isVarID .|| isVarSym

isConID  = isIdent isConID'
isVarID  = isIdent isVarID'
isConSym = isSymbol isConSym'
isVarSym = isSymbol isVarSym'

isIdent :: (String -> Bool) -> Name -> Bool
isIdent p (Ident  n) = p n
isIdent _ (Symbol _) = False

isSymbol :: (String -> Bool) -> Name -> Bool
isSymbol _ (Ident  _) = False
isSymbol p (Symbol s) = p s

isConID', isVarID', isConSym', isVarSym' :: String -> Bool
isConID'  = isConIDG (const True)
isVarID'  = isVarIDG (const True)
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

-----------------------------------------------------------------------------
-- QOp

qopGen :: CharGenMode -> Gen QOp
qopGen = qopGenWithDist defaultQOpQNameDist

qopGenWithDist :: QNameDist ->  CharGenMode -> Gen QOp
qopGenWithDist qnd cgm =
    let genOp = opGenWithDist (nameDist qnd) cgm
        liftOp :: (Name -> QName) -> Op -> QOp
        liftOp f (VarOp n) = QVarOp $ f n
        liftOp f (ConOp n) = QConOp $ f n
    in frequency
        [ (unqualf  qnd, liftOp   UnQual  <$> genOp)
        , (qualf    qnd, liftOp . Qual    <$> moduleNameGen cgm  <*> genOp)
        , (specialf qnd, QConOp . Special <$> specialConGenWithDist (specialDist qnd))
        ]

-- Op
opGen :: CharGenMode -> Gen Op
opGen = opGenWithDist defaultOpNameDist

opGenWithDist :: NameDist -> CharGenMode -> Gen Op
opGenWithDist = genFromNameWithDist ConOp VarOp

-----------------------------------------------------------------------------
-- SpecialCon

specialConGen :: Gen SpecialCon
specialConGen = specialConGenWithDist defaultSpecialConDist

data SpecialConDist
    = SCD
    { spnormalf     :: Int
    , sptuplef      :: Int
    , spboxings     :: [Boxed]
    , spmaxTSize    :: Int
    }

specialConGenWithDist :: SpecialConDist -> Gen SpecialCon
specialConGenWithDist scd = frequency
        [ (spnormalf scd , elements [UnitCon, ListCon, FunCon, Cons, UnboxedSingleCon])
        , (sptuplef  scd, TupleCon <$> elements (spboxings scd)
                                   <*> choose (2, spmaxTSize scd))
        ]

-----------------------------------------------------------------------------
-- CName, as far as it's usefull

cnameGen :: CharGenMode -> Gen CName
cnameGen = cnameGenWithDist defaultNameDist

cnameGenWithDist :: NameDist -> CharGenMode -> Gen CName
cnameGenWithDist = genFromNameWithDist ConName VarName

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
