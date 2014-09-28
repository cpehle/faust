{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies #-}
module Constraint.Solver where
import Control.Applicative
import Data.Foldable
import Data.IORef

-- data Structure a t = Structure { _structure :: a
--                                , _map :: (a -> a) -> t a -> t a
--                                                                                       , _fold :: (a -> a) -> t a -> }

{-
  Syntax of constraints:
  ======================
          C ::=
              | true
              | C /\ C
              | v = v
              | exists v.C
              | x w [witnesses?]
              | def x = v in C
              | let [vs?] C [x, v, s?]* in C
-}

data RawConstraint variable tevar 
  = CTrue
  | CConj (RawConstraint variable tevar) (RawConstraint variable tevar)
  | Ceq variable variable
  | CExist variable (RawConstraint variable tevar)
  | CInstance tevar variable (IORef [variable])         
  | CDef tevar variable (RawConstraint variable tevar)
  | CLet
  
    
-- Foldable s, Functor 

class Constrainable a where
  forAll :: a -> Constraint 
  exist  :: a -> 


-- instance Applicative Constraint where
--   pure c = Constraint (Raw.CTrue, \env -> unConstraint c)       
--   a <*> b = undefined


(~=) :: var -> var -> Constraint ()
(~=) = undefined

(~==) :: var -> str var -> Constraint ()
(~==) = undefined

exist :: (var -> Constraint a) -> Constraint (typ, var)
exist = undefined

construct :: str var -> (var -> Constraint var) -> Constraint (typ,var)
construct = undefined

exist_ :: var -> Constraint var -> Constraint var
exist_ = undefined

construct_ :: str var -> (var -> Constraint var) -> Constraint var
construct_ = undefined

