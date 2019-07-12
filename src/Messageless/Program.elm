module Messageless.Program exposing (..)

import Browser exposing (..)
import Html exposing (Html)
import Messageless.Step as Step exposing (Step)


step :
  (flags -> state) ->
  Step state () ->
  (state -> Sub (Step state ())) ->
  (state -> String) ->
  (state -> List (Html (Step state ()))) ->
  Program flags state (Step state ())
step initState initStep subscriptions title html =
  document
    {
      init = initState >> Step.toUpdate initStep,
      update = Step.toUpdate,
      subscriptions = subscriptions,
      view = \ state -> Document (title state) (html state)
    }
