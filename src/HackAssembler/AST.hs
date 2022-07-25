{-# LANGUAGE DeriveFoldable #-}

module HackAssembler.AST
  ( AInstr(..)
  , CInstr(..)
  , Exp(..)
  , EnvCtx(..)
  , Addresses
  , Dest(..)
  , Jump(..)
  , Comp(..)
  , emptyDest
  , emptyJump
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map                   as M

data AInstr
  = Symbol ByteString
  | Const Integer
  deriving (Eq, Show)

data Dest = Dest Bool Bool Bool
  deriving (Eq, Show)

emptyDest :: Dest
emptyDest = Dest False False False

data Jump = Jump Bool Bool Bool
  deriving (Eq, Show)

emptyJump :: Jump
emptyJump = Jump False False False

data Comp = Comp Bool Bool Bool Bool Bool Bool
  deriving (Eq, Show)

data CInstr
  = CInstr Bool Dest Comp Jump
  deriving (Eq, Show)

data Exp a
  = EAInstr a AInstr
  | ECInstr a CInstr
  | ELabel a ByteString
  deriving (Eq, Foldable, Show)


type Addresses = M.Map ByteString Integer


data EnvCtx = EnvCtx
  { env  :: Addresses
  , line :: Integer
  , mem  :: Integer
  } deriving (Eq, Show)
