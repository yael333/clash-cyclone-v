module Project where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes

-- Define a synthesis domain with a clock with a period of 20000 /ps/.
-- i.e. 50 MHz
-- createDomain vSystem{vName="Input", vPeriod=20000}

-- Define a synthesis domain with a clock with a period of 50000 /ps/.
-- i.e. 20 MHz
createDomain vSystem{vName="Dom20MHz", vPeriod=50000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "Project"
    , t_inputs = [ PortName "KEY0" ]
    , t_output = PortName "LED"
    }) #-}
topEntity ::
  Signal Dom20MHz Bit
    `Annotate` 'StringAttr "chip_pin" "AC9"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\"" ->

  Signal Dom20MHz Bit
    `Annotate` 'StringAttr
                "chip_pin" "F7"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\""
topEntity sw = sw