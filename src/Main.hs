{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -option #-}
module Main where

import Clash.Prelude
import Clash.Annotations.SynthesisAttributes
import Clash.Cores.UART
import Data.Char (ord)
import Control.Monad.Trans.State.Strict

-- 50 MHz
createDomain vSystem{vName="Input", vPeriod=20_000}

{-# ANN topEntity
  (Synthesize
    { t_name   = "Main"
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
    txM = (exposeClockResetEnable mealyS clk resetGen enableGen) cpu Listening (CPUIn <$> key0 <*> ack <*> rxWord)


data CPUIn = CPUIn {
  key0 :: Bit,
  ack :: Bool,
  rx :: Maybe (BitVector 8)
}

data CPUState = Transmitting (BitVector 8) | Listening deriving (Generic, NFDataX)
cpu :: CPUIn -> State CPUState (Maybe (BitVector 8))
cpu CPUIn{rx=Just rx} = do
  put $ Transmitting $ rx
  return Nothing

cpu CPUIn{ack=True} = put Listening >> return Nothing

cpu CPUIn{ack=False,rx=Nothing} = get >>= \case 
  Transmitting s -> return $ Just s
  Listening -> return Nothing