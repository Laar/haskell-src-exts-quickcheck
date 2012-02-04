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
-- | Generators for Haskell 98 compliant junk AST parts.
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
) where

-----------------------------------------------------------------------------

import Control.Applicative ((<$>), (<*>))

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

-----------------------------------------------------------------------------

