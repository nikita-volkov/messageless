module Messageless.Extensions.Program exposing (..)

import Browser exposing (..)
import Html exposing (Html)
import Messageless.StepCat as StepCat exposing (StepCat(..))
import Messageless.Step as Step exposing (Step)


step :
  (flags -> state) ->
  Step state ->
  (state -> Sub (Step state)) ->
  (state -> String) ->
  (state -> List (Html (Step state))) ->
  Program flags state (Step state)
step initState (StepCat initStepFn) subscriptions title html =
  document
    {
      init = initState >> initStepFn,
      update = \ (StepCat update) -> update,
      subscriptions = subscriptions,
      view = \ state -> Document (title state) (html state)
    }
