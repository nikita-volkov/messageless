module Messageless.Program exposing (..)

import Browser exposing (..)
import Html exposing (Html)
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
  document
    {
      init = initState >> Update.step initStep,
      update = Update.step,
      subscriptions = subscriptions,
      view = \ state -> Document (title state) (html state)
    }
