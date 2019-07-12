module Messageless.Update exposing (..)

import Browser exposing (..)
import Html exposing (Html)
import Messageless.Step as Step exposing (Step)


{-|
The update function from the standard Elm Architecture.
-}
type alias Update state = state -> (state, Cmd (Step state ()))

{-|
Convert step into the update function.
-}
step : Step state () -> Update state
step x = case x of
  Step.LoopingStep loop -> loop
  Step.EmittingStep () -> \ state -> (state, Cmd.none)
