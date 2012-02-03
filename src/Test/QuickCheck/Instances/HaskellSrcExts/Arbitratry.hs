{-# OPTIONS_GHC -fno-warn-orphans#-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Haskell.Exts.QuickCheck.Arbitratry
-- Copyright   :  (c) 2012 Lars Corbijn
-- License     :  BSD-style (see the file /LICENSE)
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Defines arbitrary instances for haskell-src-exts elements using the
-- generators and shrinkers from "Language.Haskell.Exts.QuickCheck.Generators"
--
-----------------------------------------------------------------------------

module Test.QuickCheck.Instances.HaskellSrcExts.Arbitratry (
) where

-----------------------------------------------------------------------------

import Test.QuickCheck
import Language.Haskell.Exts.Syntax

import Test.QuickCheck.Instances.HaskellSrcExts.Generators

-----------------------------------------------------------------------------
-- Module

-----------------------------------------------------------------------------
-- WarningText

instance Arbitrary WarningText where
    arbitrary = warningTextGen
    shrink    = shrinkWarningText

-----------------------------------------------------------------------------
-- ExportSpec
-- ImportDecl
-- ImportSpec

-----------------------------------------------------------------------------
-- Assoc

instance Arbitrary Assoc where
    arbitrary = assocGen

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

instance Arbitrary Boxed where
    arbitrary = boxedGen
    shrink    = shrinkBoxed

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

instance Arbitrary Literal where
    arbitrary = literalGen

-----------------------------------------------------------------------------

instance Arbitrary ModuleName where
    arbitrary = moduleNameGen Default
    shrink    = shrinkModuleName

-----------------------------------------------------------------------------
-- QName

instance Arbitrary QName where
    arbitrary = qnameGen arbitrary arbitrary arbitrary

-----------------------------------------------------------------------------
-- Name
instance Arbitrary Name where
    arbitrary = nameGen Default

-----------------------------------------------------------------------------
-- QOp

instance Arbitrary QOp where
    arbitrary = qopGen arbitrary arbitrary arbitrary
-- Op
instance Arbitrary Op where
    arbitrary = opGen Default

-----------------------------------------------------------------------------
-- SpecialCon

instance Arbitrary SpecialCon where
    arbitrary = specialConGen

-----------------------------------------------------------------------------
-- CName, as far as it's usefull
instance Arbitrary CName where
    arbitrary = cnameGen Default

-----------------------------------------------------------------------------

-- IPName
-- XName

-- Bracket
-- Splice

-----------------------------------------------------------------------------

instance Arbitrary Safety where
    arbitrary = safetyGen
    shrink    = shrinkSafety

instance Arbitrary CallConv where
    arbitrary = callConvGen

-----------------------------------------------------------------------------

-- ModulePragma
-- Tool
-- Rule
-- RuleVar
-- Activation
-- Annotation

-----------------------------------------------------------------------------
