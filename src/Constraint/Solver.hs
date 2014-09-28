{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, NullaryTypeClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
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

-- This class is superfluous, just require that a term variable has 
-- an instance of Ord
class TermVar t where
  type Var t
  compare :: Var t -> Var t -> Int 

class Output a t where
  type Structure a
  type Ty t
  variable  :: Int -> Ty t
  structure :: Structure (Ty t) -> Ty t
  mu :: Ty t -> Ty t -> Ty t
  


-- Foldable s, Functor 



-- instance Applicative Constraint where
--   pure c = Constraint (Raw.CTrue, \env -> unConstraint c)       
--   a <*> b = undefined

-- | [v ~= w] is an equality constraint on the typevariables v and w.
(~=) :: var -> var -> co ()
(~=) = undefined

-- | [v ~== w ] Analogous to [ v ~= w ], except that the right hand 
-- | side is a shallow type instead of a type variable.     
(~==) :: var -> str var -> co ()
(~==) = undefined

exist :: (var -> co a) -> co (typ, var)
exist = undefined

construct :: str var -> (var -> co var) -> co (typ, var)
construct = undefined

exist_ :: var -> co var -> co var
exist_ = undefined

construct_ :: str var -> (var ->  co var) -> co var
construct_ = undefined

lift :: (a -> var -> co b) -> a -> (str var) -> co b
lift = undefined

isinstance :: tevar -> var -> co [ty]
isinstance = undefined

def :: tevar -> var -> co a -> co a
def = undefined

let1 :: tevar -> (var -> co a) -> co b -> co ([tyvar], a, [scheme], b)              
let1 = undefined

let0 :: co a -> co ([tyvar], a)
let0 = undefined

letn :: [tevar] -> ([variable] -> co a) -> co b -> co ([tyvar], a, [scheme], b)
letn = undefined
     
solve :: Bool -> co a -> a
solve = undefined

data Ref a = Ref

data Constraint v tv s where
  CTrue     :: Constraint v tv s
  CConj     :: Constraint v tv s -> Constraint v tv s -> Constraint v tv s
  CEq       :: v -> v -> Constraint v tv s
  CExist    :: v -> Constraint v tv s -> Constraint v tv s
  CInstance :: tv -> v -> Ref [v] -> Constraint v tv s
  CDef      :: tv -> v -> Constraint v tv s
  CLet      :: Ref [v] 
            -> Constraint v tv s 
            -> [(tv, v, Ref s)] 
            -> Constraint v tv s
            -> Constraint v tv s


unify = undefined
getState = undefined
register = undefined

solve' env c = case c of
  CTrue -> undefined
  CConj c1 c2 -> do solve env c1
                    solve env c2
  CEq v w -> unify v w
  CExist v c -> do s <- getState
                   register s v
                   solve env c

