module Messageless.Cmd exposing (..)

import Task


pure : a -> Cmd a
pure a = Task.perform identity (Task.succeed a)
