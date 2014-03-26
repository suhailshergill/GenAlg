module Algorithm.GenAlg.Utils.Instances where

import Control.Monad.Random

-- | A 'Random' view for an 'Enum'
newtype (Bounded a) => Enum2Random a = Enum2Random { random2Enum :: a } deriving (Bounded)

-- | 'Random' instances for 'Enum' types which are also 'Bounded'
instance (Enum t
         ,Bounded t)
         => Random (Enum2Random t) where
  randomR ((Enum2Random a), (Enum2Random b)) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (Enum2Random . toEnum $ x, g')
  random g = randomR (Enum2Random minBound, Enum2Random maxBound) g
