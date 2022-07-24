{-# LANGUAGE DeriveFoldable #-}

module HackAssembler.AST (AInstr(..), CInstr(..), Exp(..)) where

import           Data.ByteString.Lazy.Char8 (ByteString)

data AInstr
  = Symbol ByteString
  | Const Integer
  deriving (Eq, Show)

data CInstr
  = CInstr Bool (Maybe ByteString) ByteString (Maybe ByteString)
  deriving (Eq, Show)

data Exp a
  = EAInstr a AInstr
  | ECInstr a CInstr
  | ELabel a ByteString
  deriving (Eq, Foldable, Show)
