module Messageless.Signal exposing (..)

import Messageless.Step as Step exposing (Step)
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Monocle.Prism as Prism exposing (Prism)
import Task exposing (Task)


type alias Signal state notification = Step state (List notification)

step : Step a b -> Signal a b
step = Step.map List.singleton

unitStep : Step a () -> Signal a b
unitStep = Step.map (always [])

empty : Signal state a
empty = Step.pure []

map : (a -> b) -> Signal state a -> Signal state b
map aToB = Step.map (List.map aToB)

map2 : (a -> b -> c) -> Signal state a -> Signal state b -> Signal state c
map2 aToBToC = Step.map2 (List.map2 aToBToC)

pure : a -> Signal state a
pure = Step.pure << List.singleton

and : Signal state a -> Signal state b -> Signal state a
and = Step.and

traverse : (a -> Signal state b) -> List a -> Signal state (List b)
traverse aToSignalB = Step.traverse aToSignalB

get : Signal state state
get = step Step.get

put : state -> Signal state a
put state = unitStep (Step.put state)

access : (state -> result) -> Signal state result
access fn = step (Step.access fn)

modify : (state -> state) -> Signal state a
modify fn = unitStep (Step.modify fn)

accessAndModify : (state -> result) -> (state -> state) -> Signal state result
accessAndModify accessProj modifyProj = step (Step.accessAndModify accessProj modifyProj)

interact : (state -> (result, state)) -> Signal state result
interact fn = step (Step.interact fn)

cmd : (state -> Cmd result) -> Signal state result
cmd fn = step (Step.cmd fn)

task : (state -> Task Never result) -> Signal state result
task x = step (Step.task x)

zoomWithLens : Lens b a -> Signal a result -> Signal b result
zoomWithLens = Step.zoomWithLens

zoomWithOptional : Optional b a -> Signal a result -> Signal b result
zoomWithOptional optional = Step.zoomWithOptional optional >> Step.map (Maybe.withDefault [])

zoomWithPrism : Prism b a -> Signal a result -> Signal b result
zoomWithPrism prism = Step.zoomWithPrism prism >> Step.map (Maybe.withDefault [])
