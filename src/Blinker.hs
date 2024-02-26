{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -option #-}
module Blinker where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes
import Clash.Cores.UART
import Control.Monad.Trans.State.Strict
import I2C (i2cMaster, Message)


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
  -> "I2C_SCL" ::: BiSignalIn 'PullUp Input (BitSize Bit)
    `Annotate` 'StringAttr
                "chip_pin B7"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"2.5V\""
  -> "I2C_SDA" ::: BiSignalIn 'PullUp Input (BitSize Bit)
    `Annotate` 'StringAttr
                "chip_pin G11"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"2.5V\""
  -> (Signal Input Bit
    `Annotate` 'StringAttr
                "chip_pin" "L9"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"2.5V\""
    ,"I2C_SCL" ::: BiSignalIn 'PullUp Input (BitSize Bit)
    `Annotate` 'StringAttr
                "chip_pin B7"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"2.5V\""
    ,"I2C_SDA" ::: BiSignalIn 'PullUp Input (BitSize Bit)
    `Annotate` 'StringAttr
                "chip_pin G11"
    `Annotate` 'StringAttr
                "altera_attribute" "-name IO_STANDARD \"2.5V\""
    )
topEntity clk rx key0 sclIn sdaIn = (txBit,sclOut,sdaOut)
  where
    baud = SNat @115200
    uart' = exposeClockResetEnable (uart baud) clk resetGen enableGen
    (rxWord, txBit, ackUART) = uart' rx txM
    (sclOut, sdaOut, ackI2C) = exposeClockResetEnable (i2cMaster (SNat @20_000) i2cM sclIn sdaIn) clk resetGen enableGen
    (txM,i2cM) = exposeClockResetEnable mealyS clk resetGen enableGen cpu Initialization (CPUIn <$> key0 <*> (ackUART,ackI2C) <*> rxWord)


data CPUIn = CPUIn {
  key0 :: Bit,
  ackUART :: Bool,
  ackI2C :: Bool,
  rx :: Maybe (BitVector 8)
}

type CPUOut = (Maybe (BitVector 8),Maybe Message)

data CPUState = Initialization
                | TransmittingUART (BitVector 8)
                | TransmittingI2C Message
                | Listening 
                deriving (Generic, NFDataX)
cpu :: CPUIn -> State CPUState CPUOut
cpu CPUIn{rx=Just rx} = do
  put $ Transmitting rx
  return (Nothing, Nothing)

cpu CPUIn{ackUART=True} = put Listening >> return (Nothing, Nothing)
cpu CPUIn{ackI2C=True} = put Listening >> return (Nothing, Nothing)

cpu _ = get >>= \case
  Initialization -> put Listening >> return (Nothing, Nothing)
  TransmittingUART s -> return (Just s, Nothing)
  TransmittingI2C s -> return (Nothing, Just s)
  Listening -> return (Nothing, Nothing)
