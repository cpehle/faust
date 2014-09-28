{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}        
-- | 
module CodeGeneration where
       
import Control.Applicative
import Control.Monad.State.Strict
import Data.Word
import Data.Sequence ( Seq )
import LLVM.General.AST
import Data.String

instance IsString Name where
  fromString = Name . fromString

data PTXMemorySpace
  = Generic
  | Global
  | InternalUse
  | Shared
  | Constant
  | Local
    
data CodeGenState = CodeGenState { _blockChain :: Seq Block
                                 , _next :: {-# UNPACK #-} !Word
                                 }
        
data Block = Block { _blockLabel   :: Name
                   , _instructions :: Seq (Named Instruction)      
                   , _terminator   :: Maybe (Named Terminator)
                   }
     
newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)

