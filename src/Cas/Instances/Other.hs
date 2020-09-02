module Cas.Instances.Other where

import PreludeCustom
import Cas.Internal.Interface
import Cas.Internal.Normalization
import Data.Complex.Generic


instance Enum X where
    fromEnum = xI
    toEnum = X


instance Fractional T where
    recip = normalize . mInv
    fromRational = TF . fromRational

instance Num T where
    negate      = normalize . aInv
    (+)         = (normalize .) .  addBin
    (*)         = (normalize .) .  mulBin
    fromInteger = TF . fromInteger

instance Floating T where
    (**) = (normalize .) . pow

-- instance Fractional T where
--     recip = mInv
--     fromRational = TF . fromRational

-- instance Num T where
--     negate      =  aInv
--     (+)         =  addBin
--     (*)         =  mulBin
--     fromInteger = TF . fromInteger

-- instance Floating T where
--     (**) = pow


