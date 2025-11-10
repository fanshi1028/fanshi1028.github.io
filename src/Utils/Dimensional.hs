{-# LANGUAGE NoImplicitPrelude #-}

module Utils.Dimensional where

import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.SIUnits
import Prelude (Fractional)

----------------------------------------------------------------------------------------
-- NOTE: copied from Numeric.Units.Dimensional.SIUnits but weaken Float -> Fractional --
----------------------------------------------------------------------------------------
toDegreeCelsiusAbsolute :: (Fractional a) => ThermodynamicTemperature a -> a
toDegreeCelsiusAbsolute x = (x - 273.15 *~ degreeCelsius) /~ degreeCelsius
