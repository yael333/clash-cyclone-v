{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -option #-}
module Blinker where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes
import Clash.Cores.UART
import Data.Char (ord)
import Control.Monad.Trans.State.Strict

-- 50 MHz
createDomain vSystem{vName="Input", vPeriod=20_000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "Blinker"
    , t_inputs = [
      PortName "CLK0",
      PortName "UART_RX"
       ]
    , t_output = PortName "UART_TX"
    }) #-}
topEntity ::
  "CLK" ::: Clock Input
    `Annotate` 'StringAttr "chip_pin" "R20"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"3.3-V LVTTL\""
  -> "RX" ::: Signal Input Bit
    `Annotate` 'StringAttr
                "chip_pin" "M9"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"2.5V\""
  -> "KEY0" ::: Signal Input Bit
    `Annotate` 'StringAttr
                "chip_pin" "P11"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"1.2V\""
  -> Signal Input Bit
    `Annotate` 'StringAttr
                "chip_pin" "L9"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"2.5V\""
topEntity clk rx key0 = txBit
  where
    baud = SNat @115200
    uart' = exposeClockResetEnable (uart baud) clk resetGen enableGen
    (rxWord, txBit, ack) = uart' rx txM
    txM = (exposeClockResetEnable mealySB clk resetGen enableGen) (uncurry cpu) Nothing (ack,rxWord)

type CpuState = Maybe (BitVector 8)
cpu :: Bool -> Maybe (BitVector 8) -> State CpuState (Maybe (BitVector 8))
cpu _ (Just rx) = do
  put (Just rx)
  return Nothing

cpu True Nothing = do
  put Nothing
  return Nothing

cpu False Nothing = do
  s <- get
  return s