-- | 

module Constraint.Term where
       
import qualified Syntax

constrain env term ty  = case term of
  Literal lit -> Literal.constrain env lit ty
