Polar Game Engine
======

Polar Game Engine is a general-purpose, modern, safe game engine written in the functional programming language Haskell.

## Architecture

The engine is comprised of three main abstraction layers.

The bottom layer is called Core and represents the inner workings of the engine. Core is not intended to be used by application developers as it has the full ability to perform any IO at any time.

The middle layer is called Sys and represents the operations systems of the engine, such as graphics renderer, audio mixer, physics integrator, and more. Sys is intended to be used by low-level application code that must operate across a subset of engine objects every tick. Sys can only perform IO through actions propagated down to Core, which Core may schedule and execute as needed.

The top layer is called Logic and represents the application logic of the engine. Logic should be used by default for most application code unless there is a strong reason to use Sys. Logic cannot perform or propagate IO and instead can only propogate a subset of actions down to the Sys layer for them to be handled appropriately.

## Actions

| SysAction                          | Description                                                     |
| ---------------------------------- | --------------------------------------------------------------- |
| SysExitAction                      | shutdown and exit                                               |
| SysLogWriteAction Priority Message | write to log if Priority is higher than Log.Level config option |
| SysCoreAction Core                 | directly execute Core action (unsafe, backdoor)                 |
