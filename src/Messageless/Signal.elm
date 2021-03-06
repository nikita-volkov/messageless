module Messageless.Signal exposing (..)

import Messageless.Step as Step exposing (Step)
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Monocle.Prism as Prism exposing (Prism)
import Task exposing (Task)


type alias Signal state notification = Step state (List notification)

run : Signal state notification -> state -> (List notification, state, Cmd (Signal state notification))
run signal = case signal of
  Step.LoopingStep loop -> \ state -> case loop state of
    (nextState, cmd_) -> ([], nextState, cmd_)
  Step.EmittingStep notifications -> \ state -> (notifications, state, Cmd.none)

step : Step a b -> Signal a b
step = Step.map List.singleton

unitStep : Step a () -> Signal a b
unitStep = Step.map (always [])

empty : Signal state a
empty = Step.pure []

maybe : Signal s a -> Signal s (Maybe a)
maybe = Step.map (\ list -> if List.isEmpty list then [Nothing] else List.map Just list)

ignore : Signal state a -> Signal state b
ignore = Step.map (always [])

map : (a -> b) -> Signal state a -> Signal state b
map aToB = Step.map (List.map aToB)

map2 : (a -> b -> c) -> Signal state a -> Signal state b -> Signal state c
map2 aToBToC = Step.map2 (List.map2 aToBToC)

pure : a -> Signal state a
pure = Step.pure << List.singleton

and : Signal state a -> Signal state a -> Signal state a
and secondSignal = Step.andThen (\ a -> Step.map ((++) a) secondSignal)

andThen : (a -> Signal state b) -> Signal state a -> Signal state b
andThen aToSignalB = Step.andThen (Step.traverse aToSignalB >> Step.map List.concat)

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

attemptTask : (state -> Task err result) -> Signal state (Result err result)
attemptTask fn = step (Step.cmd (fn >> Task.attempt identity))

zoomWithLens : Lens b a -> Signal a result -> Signal b result
zoomWithLens = Step.zoomWithLens

zoomWithOptional : Optional b a -> Signal a result -> Signal b result
zoomWithOptional optional = Step.zoomWithOptional optional >> Step.map (Maybe.withDefault [])

zoomWithPrism : Prism b a -> Signal a result -> Signal b result
zoomWithPrism prism = Step.zoomWithPrism prism >> Step.map (Maybe.withDefault [])
