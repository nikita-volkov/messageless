module Messageless.Step exposing (..)

import Messageless.StepCat as StepCat exposing (StepCat(..))
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Monocle.Prism as Prism exposing (Prism)
import Task exposing (Task)


{-|
A simpler variation of StepCat,
abstracting over the changing of the type thru actions.

You're likely to never need the more complicated basis API of StepCat.
-}
type alias Step state = StepCat state state

empty : Step state
empty = StepCat.identity

always : state -> Step state
always = StepCat.always

modify : (state -> state) -> Step state
modify = StepCat.modify

cmd : (state -> Cmd (Step state)) -> Step state
cmd = StepCat.cmd

task : (state -> Task Never (Step state)) -> Step state
task = StepCat.task

modifyAndCmd : (state -> state) -> (state -> Cmd (Step state)) -> Step state
modifyAndCmd = StepCat.modifyAndCmd

modifyAndTask : (state -> state) -> (state -> Task Never (Step state)) -> Step state
modifyAndTask = StepCat.modifyAndTask

batch : List (Step state) -> Step state
batch = StepCat.batch

map : (a -> b) -> (b -> a) -> Step a -> Step b
map = StepCat.mapBoth

zoomWithLens : Lens b a -> Step a -> Step b
zoomWithLens lens (StepCat stepFn) = StepCat <| \ b ->
  lens.get b |> stepFn |> Tuple.mapBoth (\ a -> lens.set a b) (Cmd.map (zoomWithLens lens))

zoomWithOptional : Optional b a -> Step a -> Step b
zoomWithOptional optional (StepCat stepFn) = StepCat <| \ b ->
  case optional.getOption b of
    Just a -> stepFn a |> Tuple.mapBoth (\ newA -> optional.set newA b) (Cmd.map (zoomWithOptional optional))
    Nothing -> (b, Cmd.none)

zoomWithPrism : Prism b a -> Step a -> Step b
zoomWithPrism prism (StepCat stepFn) = StepCat <| \ b ->
  case prism.getOption b of
    Just a -> stepFn a |> Tuple.mapBoth (\ newA -> prism.reverseGet newA) (Cmd.map (zoomWithPrism prism))
    Nothing -> (b, Cmd.none)
