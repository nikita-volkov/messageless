module Messageless.StepCat exposing (..)
{-|
This API builds up on an idea that messages in Elm
surve no other purpose but to be interpreted
as an update to the model paired with a side-effect (Cmd).

Interpreting messages requires a redundant logic
involving pattern-matching, which results in a computation,
which can be eliminated, extra code and a burden of
the extra type for messages to model and carry around.

With the approach presented by this library we eliminate
the requirement for message data-types altogether
by utilising recursive references to step-functions.
-}

import Task exposing (Task)


{-|
Step category.
An abstraction over a function, which does two things:

- Accesseses and modifies the state;
- Generates a command, which itself produces another step instead of a message.

-}
type StepCat input output = StepCat (input -> (output, Cmd (StepCat input output)))

identity : StepCat a a
identity = StepCat (\ a -> (a, Cmd.none))

always : b -> StepCat a b
always b = StepCat (\ _ -> (b, Cmd.none))

modify : (a -> b) -> StepCat a b
modify aToB = StepCat (\ a -> (aToB a, Cmd.none))

mapInput : (b -> a) -> StepCat a output -> StepCat b output
mapInput bToA (StepCat stepFnA) = StepCat <| bToA >> stepFnA >> Tuple.mapSecond (Cmd.map (mapInput bToA))

mapOutput : (a -> b) -> StepCat input a -> StepCat input b
mapOutput aToB (StepCat stepFnA) = StepCat <| stepFnA >> Tuple.mapBoth aToB (Cmd.map (mapOutput aToB))

mapBoth : (a -> b) -> (d -> c) -> StepCat c a -> StepCat d b
mapBoth aToB dToC (StepCat stepFnAToC) = StepCat <| dToC >> stepFnAToC >> Tuple.mapBoth aToB (Cmd.map (mapBoth aToB dToC))

precede : StepCat a b -> StepCat b c -> StepCat a c
precede aToB bToC = succeed bToC aToB

succeed : StepCat b c -> StepCat a b -> StepCat a c
succeed (StepCat stepFnBToC) (StepCat stepFnAToB) = StepCat <| \ a ->
  let
    (b, cmdAToB) = stepFnAToB a
    (c, cmdBToC) = stepFnBToC b
    cmdAToC =
      Cmd.batch
        [
          Cmd.map (succeed (StepCat stepFnBToC)) cmdAToB,
          Cmd.map (precede (StepCat stepFnAToB)) cmdBToC
        ]
    in (c, cmdAToC)

batch : List (StepCat a a) -> StepCat a a
batch = List.foldl succeed identity

cmd : (a -> Cmd (StepCat a a)) -> StepCat a a
cmd x = StepCat (\ a -> (a, x a))

task : (a -> Task Never (StepCat a a)) -> StepCat a a
task x = StepCat (\ a -> (a, Task.perform Basics.identity (x a)))

modifyAndCmd : (a -> b) -> (a -> Cmd (StepCat a b)) -> StepCat a b
modifyAndCmd aToB aToCmdAToB = StepCat (\ a -> (aToB a, aToCmdAToB a))

modifyAndTask : (a -> b) -> (a -> Task Never (StepCat a b)) -> StepCat a b
modifyAndTask aToB aToTaskAToB = StepCat (\ a -> (aToB a, Task.perform Basics.identity (aToTaskAToB a)))
