module Messageless.Step exposing (..)

import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Monocle.Prism as Prism exposing (Prism)
import Task exposing (Task)
import Messageless.Cmd as Cmd

{-|
Composable abstraction over a function, which does two things:

- Accesseses and modifies the state;
- Generates a command, which itself produces another step instead of a message.

-}
type Step state result = 
  LoopingStep (state -> (state, Cmd (Step state result))) |
  EmittingStep result

map : (a -> b) -> Step state a -> Step state b
map aToB step = case step of
  LoopingStep loop -> LoopingStep <| loop >> (aToB |> map |> Cmd.map |> Tuple.mapSecond)
  EmittingStep a -> EmittingStep (aToB a)

pure : a -> Step state a
pure = EmittingStep

and : Step state a -> Step state () -> Step state a
and secondStep firstStep = case firstStep of
  LoopingStep firstLoop -> LoopingStep <| firstLoop >> Tuple.mapSecond (Cmd.map (and secondStep))
  EmittingStep _ -> secondStep

andThen : (a -> Step state b) -> Step state a -> Step state b
andThen aToStepB step = case step of
  LoopingStep loop -> LoopingStep <| loop >> Tuple.mapSecond (Cmd.map (andThen aToStepB))
  EmittingStep a -> aToStepB a

{-| A flipped version of `andThen`. -}
executing : Step state a -> (a -> Step state b) -> Step state b
executing stepA aToStepB = andThen aToStepB stepA

ignore : Step state a -> Step state ()
ignore = map (always ())

get : Step state state
get = LoopingStep <| \ state -> (state, Cmd.pure (EmittingStep state))

put : state -> Step state ()
put state = LoopingStep <| \ _ -> (state, Cmd.pure (EmittingStep ()))

access : (state -> result) -> Step state result
access fn = LoopingStep <| \ state -> (state, Cmd.pure (EmittingStep (fn state)))

modify : (state -> state) -> Step state ()
modify fn = LoopingStep (\ state -> (fn state, Cmd.pure (EmittingStep ())))

interact : (state -> (result, state)) -> Step state result
interact fn = LoopingStep (fn >> \ (result, state) -> (state, Cmd.pure (EmittingStep result)))

cmd : (state -> Cmd result) -> Step state result
cmd fn = LoopingStep <| \ state ->
  fn state |>
  Cmd.map EmittingStep |>
  Tuple.pair state

task : (state -> Task Never result) -> Step state result
task x = LoopingStep (\ state -> (state, Task.perform EmittingStep (x state)))

modifyAndCmd : (state -> state) -> (state -> Cmd result) -> Step state result
modifyAndCmd stateFn cmdFn = LoopingStep (\ state -> (stateFn state, Cmd.map EmittingStep (cmdFn state)))

modifyAndTask : (state -> state) -> (state -> Task Never result) -> Step state result
modifyAndTask stateFn taskFn = LoopingStep (\ state -> (stateFn state, Task.perform EmittingStep (taskFn state)))

zoomWithLens : Lens b a -> Step a result -> Step b result
zoomWithLens lens step = case step of
  EmittingStep result -> EmittingStep result
  LoopingStep loop -> LoopingStep <| \ b ->
    lens.get b |> loop |> Tuple.mapBoth (\ a -> lens.set a b) (Cmd.map (zoomWithLens lens))

zoomWithOptional : Optional b a -> Step a result -> Step b (Maybe result)
zoomWithOptional optional step = case step of
  LoopingStep loop -> LoopingStep <| \ b ->
    case optional.getOption b of
      Just a -> loop a |> Tuple.mapBoth (\ newA -> optional.set newA b) (Cmd.map (zoomWithOptional optional))
      Nothing -> (b, Cmd.pure (EmittingStep Nothing))
  EmittingStep result -> EmittingStep (Just result)

zoomWithPrism : Prism b a -> Step a result -> Step b (Maybe result)
zoomWithPrism prism step = case step of
  LoopingStep loop -> LoopingStep <| \ b ->
    case prism.getOption b of
      Just a -> loop a |> Tuple.mapBoth (\ newA -> prism.reverseGet newA) (Cmd.map (zoomWithPrism prism))
      Nothing -> (b, Cmd.pure (EmittingStep Nothing))
  EmittingStep result -> EmittingStep (Just result)
