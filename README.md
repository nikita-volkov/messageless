# Summary

An alternative approach to Elm's effect system,
which eradicates the need for the Msg type altogether and
provides for composition of effects in-between the cycles of a program.

# Motivation

The core idea behind this library is that messages in Elm
surve no other purpose but to be interpreted
as an update to the model paired with a side-effect (Cmd).
So why not just pass around the immediate references to the update functions instead of messages.

Interpreting messages requires redundant logic
involving pattern-matching, which results in extra computation,
extra code and a burden of an extra type for messages to model, maintain and carry around.

With the approach presented by this library we eliminate
the requirement for message data-types altogether
by utilising recursive references to step-functions.
