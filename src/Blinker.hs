{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Blinker where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes

createDomain vSystem{vName="Input", vPeriod=20000}

-- createDomain vSystem{vName="Dom20MHz", vPeriod=50000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "Blinker"
    , t_inputs = [
      PortName "CLK0",
      PortName "KEY0"
       ]
    , t_output = PortName "LED"
    }) #-}
topEntity ::
  HiddenClockResetEnable Input =>
  Clock Input
    `Annotate` 'StringAttr "chip_pin" "R20"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\"" ->
  Signal Input Bit
    `Annotate` 'StringAttr "chip_pin" "AC9"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\"" ->

  Signal Input Bit
    `Annotate` 'StringAttr
                "chip_pin" "F7"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\""
topEntity clk sw = msb <$> r
  where
    r :: Signal Input (Unsigned 24)
    r = withClockResetEnable clk resetGen enableGen $ register 0 (r + 1)


-- graph :: Signal Dom20MHz (Unsigned 8) -> Signal Dom20MHz (Maybe ((Unsigned 8), Term)) -> Signal Dom20MHz Term
-- graph = asyncRam d32