-- | 
module Constraint.Internal.Solver where

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

data WriteOnceRef a


data Constraint variable tevar 
  = CTrue
  | CConj (Constraint variable tevar) (Constraint variable tevar)
  | Ceq variable variable
  | CExist variable (Constraint variable tevar)
  | CInstance tevar variable (WriteOnceRef [variable])         
  | CDef tevar variable (Constraint variable tevar)
  | CLet
