module Messageless.Program exposing (..)

import Browser exposing (..)
import Html exposing (Html)
import Messageless.Signal as Signal exposing (Signal)
import Messageless.Step as Step exposing (Step)
import Messageless.Update as Update


step :
  (flags -> state) ->
  Step state () ->
  (state -> Sub (Step state ())) ->
  (state -> String) ->
  (state -> List (Html (Step state ()))) ->
  Program flags state (Step state ())
step initState initStep subscriptions title html =
  document {
      init = initState >> Update.step initStep,
      update = Update.step,
      subscriptions = subscriptions,
      view = \ state -> Document (title state) (html state)
    }

signal :
  (flags -> state) ->
  Signal state a ->
  (state -> Sub (Signal state a)) ->
  (state -> String) ->
  (state -> List (Html (Signal state a))) ->
  Program flags state (Signal state a)
signal initState initStep subscriptions title html =
  let
    signalToUpdate x = case x of
      Step.LoopingStep loop -> loop
      Step.EmittingStep _ -> \ state -> (state, Cmd.none)
    in document {
        init = initState >> signalToUpdate initStep,
        update = signalToUpdate,
        subscriptions = subscriptions,
        view = \ state -> Document (title state) (html state)
      }
