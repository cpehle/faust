{-# LANGUAGE RankNTypes #-}
module Syntax where

import Data.IORef
import qualified LLVM.General.AST.Type as LLVM


type Name = String

data Literal
  = Integer Integer
  | Rational Rational
  | String String
  | Boolean Bool
  deriving Show

data Term
  = Variable Name                                   
  | ALam Name Sigma Term
  | Lam Name Term
  | Ann Term Sigma
  | Let Name Term Term
  | App Term Term
  | Op Name [Term] 
  | Lit Literal
  | Atom String
  | Assignment [Name] Term
  deriving Show
           
type Unique = Int

data TyVar
  = BoundTv String		-- A type variable bound by a ForAll
  | SkolemTv String Unique	-- A skolem constant
  deriving Show                 


data Type
  = ForAll [TyVar] Rho
  | Fun Type Type
  | TyCon TyCon
  | TyLLVM LLVM.Type 
  | TyVar TyVar
  | MetaTv MetaTv
  deriving Show

data TyCon = IntegerT | BooleanT deriving Show                                
data MetaTv = Meta Unique TyRef -- Can unify with any tau-type
instance Show MetaTv where
  show (Meta u _) = "$" ++ show u

type TyRef = IORef (Maybe Tau)
                    
        -- 'Nothing' means the type variable is not substituted
        -- 'Just ty' means it has been substituted by 'ty'
type Sigma = Type
type Rho = Type     
type Tau = Type
