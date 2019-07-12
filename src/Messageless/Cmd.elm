module Messageless.Cmd exposing (..)


pure : a -> Cmd a
pure a = Cmd.map (always a) Cmd.none
