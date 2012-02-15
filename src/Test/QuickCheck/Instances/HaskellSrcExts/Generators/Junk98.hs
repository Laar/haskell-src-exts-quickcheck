-----------------------------------------------------------------------------
--
-- Module      :  Test.QuickCheck.Instances.HaskellSrcExts.Generators.Junk98
-- Copyright   :  (c) 2012 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Generators for Haskell 98 compliant junk AST parts. These generators
-- are (absolutely) not generating realistic AST parts for checking general
-- properties. The two things you can count on is that the parts are valid
-- H98 and that they are finite. It is recommended to take a look at the
-- output using `sample` before using them.
--
-----------------------------------------------------------------------------

module Test.QuickCheck.Instances.HaskellSrcExts.Generators.Junk98 (

    -- * Module
    -- ** ExportSpec
    ESpecDist(..), defaultESpecDist,
    exportSpecJunkGen, exportSpecJunkGenWithDist,
    -- ** ImportDecl
    IDeclDist(..), defaultIDeclDist,
    importDeclJunkGen, importDeclJunkGenWithDist,
    -- ** ImportSpec
    ISpecDist(..), defaultISpecDist,
    importSpecJunkGen, importSpecJunkGenWithDist,


    -- * Type classes and instances
    -- ** Type
    TypeDist(..), defaultTypeDist,
    typeJunkGenWithDist, typeJunkGen,
    -- ** Deriving
    derivingJunkGen,
) where

-----------------------------------------------------------------------------

import Control.Applicative ((<$>), (<*>))
import Data.Maybe(catMaybes)

import Language.Haskell.Exts.Syntax
import Test.QuickCheck
import Test.QuickCheck.Instances.HaskellSrcExts.Generators

-----------------------------------------------------------------------------
-- ExportSpec

data ESpecDist
    = ESD
    { qndist :: QNameDist
    , evarf  :: Int -- ^ `EVar`
    , eabsf  :: Int -- ^ `EAbs`
    , eallf  :: Int -- ^ `EThingAll`
    , eqithf :: Int -- ^ `EThingWith`
    , emodcf :: Int -- ^ `EModuleContents`
    }

defaultESpecDist :: ESpecDist
defaultESpecDist = ESD defaultQNameDist 9 3 6 3 1

exportSpecJunkGen :: CharGenMode -> Gen ExportSpec
exportSpecJunkGen = exportSpecJunkGenWithDist defaultESpecDist

exportSpecJunkGenWithDist :: ESpecDist -> CharGenMode ->  Gen ExportSpec
exportSpecJunkGenWithDist esd cgm =
    let qng = qnameGenWithDist (qndist esd) cgm
    in frequency
        [ (evarf  esd, EVar <$> qng)
        , (eabsf  esd, EAbs <$> qng)
        , (eallf  esd, EThingAll <$> qng)
        , (eqithf esd, EThingWith <$> qng <*> plistOf1 (cnameGen cgm))
        , (emodcf esd, EModuleContents <$> moduleNameGen cgm)
        ]

-----------------------------------------------------------------------------
-- ImportDecl

data IDeclDist
    = IDD
    { iqasf  :: Int -- ^ qualified as
    , iasf   :: Int -- ^ as
    , iquf   :: Int -- ^ qualified
    , inorf  :: Int -- ^ no qualified nor as
    , ihidpf :: Int -- ^ import with hiding
    , iallpf :: Int -- ^ import all things
    , ipartf :: Int -- ^ import some
    , ispecd :: ISpecDist
    }

defaultIDeclDist :: IDeclDist
defaultIDeclDist = IDD 4 3 1 5  1 3 2 defaultISpecDist

importDeclJunkGen :: CharGenMode -> Gen ImportDecl
importDeclJunkGen = importDeclJunkGenWithDist defaultIDeclDist


importDeclJunkGenWithDist :: IDeclDist -> CharGenMode -> Gen ImportDecl
importDeclJunkGenWithDist idd cgm =
    let specjgen = importSpecJunkGenWithDist (ispecd idd) cgm
        noSrcLoc' = SrcLoc "Junk location" 0 0
    in do
        mn <- moduleNameGen cgm
        specs <- frequency
            [ (iallpf  idd, return Nothing)
            , (ipartf idd, (Just . (,) False) <$> plistOf1 specjgen)
            , (ihidpf  idd, (Just . (,) True ) <$> plistOf1 specjgen)
            ]
        (q,a) <- frequency
            [ (iqasf idd, ((,) True  . Just) <$> moduleNameGen cgm)
            , (iasf  idd, ((,) False . Just) <$> moduleNameGen cgm)
            , (iquf  idd, return (True,  Nothing))
            , (inorf idd, return (False, Nothing))
            ]
        return $ ImportDecl noSrcLoc' mn q False Nothing a specs

-----------------------------------------------------------------------------
-- ImportSpec

data ISpecDist
    = ISD
    { ivarf  :: Int -- ^ `IVar`
    , iabsf  :: Int -- ^ `IAbs`
    , iallf  :: Int -- ^ `IThingAll`
    , iwithf :: Int -- ^ `IThingWith`
    }

defaultISpecDist :: ISpecDist
defaultISpecDist = ISD 2 1 1 1

importSpecJunkGen :: CharGenMode -> Gen ImportSpec
importSpecJunkGen = importSpecJunkGenWithDist defaultISpecDist

importSpecJunkGenWithDist :: ISpecDist -> CharGenMode -> Gen ImportSpec
importSpecJunkGenWithDist isd cgm = frequency
    [ (ivarf  isd, IVar <$> nameGen cgm)
    , (iabsf  isd, IAbs <$> nameGen cgm)
    , (iallf  isd, IThingAll <$> nameGen cgm)
    , (iwithf isd, IThingWith <$> nameGen cgm <*> plistOf1 (cnameGen cgm))
    ]

-----------------------------------------------------------------------------
-- Type

tqnameDist :: QNameDist
tqnameDist = QND 10 3 0 (ND 9 0 1 0) (SCD 1 3 [Boxed] 3)

data TypeDist
    = TD
    { tinitfunclenf :: [(Int, Int)]
    , tqnamed :: QNameDist
    , tconf   :: Int
    , tvconf  :: Int
    , tvarf   :: Int
    , tparenf :: Int
    , tfunf   :: Int
    , tlistf  :: Int
    , ttupf   :: Int
    , tmaxlen :: Int
    }

-- | WARNING this doesn't generate special names
defaultTypeDist :: TypeDist
defaultTypeDist =
    TD [(1,1), (3, 2), (4, 3), (5, 4), (3,5), (1,6), (1,7)]
        tqnameDist
        5 1 3 1 1 1 1
        3

-- | WARNING this doesn't generate special names
typeJunkGen :: CharGenMode -> Gen Type
typeJunkGen = typeJunkGenWithDist defaultTypeDist

typeJunkGenWithDist :: TypeDist -> CharGenMode -> Gen Type
typeJunkGenWithDist td cgm = do
    n <- frequency . map (\(f,n') -> (f, return n')) $ tinitfunclenf td
    ts <- sequence $ replicate n (tgen (tmaxlen td))
    return $ foldr1 TyFun ts
    where
        qnamegen = qnameGenWithDist (tqnamed td) cgm
        tgen i = frequency
            [ (tvarf td     , TyVar <$> varIDGen cgm)
            , (limit tconf  , TyCon <$> qnamegen >>= conWith)
            , (limit tvconf , TyVar <$> varIDGen cgm >>= conWith)
            , (limit tparenf, TyParen <$> tgen (i-1))
            , (limit tfunf  , TyFun <$> tgen (i-1) <*> tgen (i-1))
            , (limit tlistf , TyList <$> tgen (i-1))
            , (limit ttupf  , tupgen)
            ]
            where
                conWith n = do
                    l <- choose (0, i)
                    ts <- sequence . replicate l $  tgen (i-1)
                    return $ foldr TyApp n ts
                tupgen = do
                    l <- choose (2,2+i)
                    ts <- sequence . replicate l $  tgen (i-1)
                    return $ TyTuple Boxed ts
                limit f = if i <= 0 then 0 else f td

-----------------------------------------------------------------------------
-- Deriving

derivingJunkGen
    :: Bool -- ^ qualified
    -> Bool -- ^ with `Enum` and `Bounded` possibility
    -> Gen [Deriving]
derivingJunkGen q eb = (map (\d -> (d, [])) . flip select ders) <$> vectorOf (length ders) arbitrary
    where
        ders = map ((if q then Qual (ModuleName "Prelude") else UnQual) . Ident)
            (["Eq", "Ord", "Show", "Read"]  ++ if eb then ["Enum", "Bounded"] else [])
        select :: [Bool] -> [a] -> [a]
        select bs = catMaybes . zipWith (\b -> if b then Just else const Nothing) bs

-----------------------------------------------------------------------------
