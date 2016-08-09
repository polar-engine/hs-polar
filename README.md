Polar Game Engine
======

Polar Game Engine is a general-purpose, modern, safe game engine written in the functional programming language Haskell.

## Architecture

The engine is comprised of three main abstraction layers.

The top layer is called Logic and represents the application logic of the engine. Logic should be used by default for most application code unless there is a strong reason to use Sys. Logic cannot perform or propagate IO and instead can only propogate a subset of actions down to the Sys layer for them to be handled appropriately.

The middle layer is called Sys and represents the operations systems of the engine, such as graphics renderer, audio mixer, physics integrator, and more. Sys is intended to be used by low-level application code that must operate across a subset of engine objects every tick. Sys can only perform IO through actions propagated down to Core, which Core may schedule and execute as needed.

The bottom layer is called Core and represents the inner workings of the engine. Core is not intended to be used by application developers as it has the full ability to perform any IO at any time.

## Actions

| LogicAction                          | Description                                                     |
| ------------------------------------ | --------------------------------------------------------------- |
| LogicExitAction                      | shutdown and exit                                               |
| LogicLogWriteAction Priority Message | write to log if Priority is higher than Log.Level config option |

| SysAction                          | Description                                                     |
| ---------------------------------- | --------------------------------------------------------------- |
| SysExitAction                      | shutdown and exit                                               |
| SysLogWriteAction Priority Message | write to log if Priority is higher than Log.Level config option |
| SysCoreAction Core                 | directly execute Core action (unsafe, backdoor)                 |

## Writing a System

### Hello World

###### `Hello.hs`
```haskell
module Hello (hello) where

import Polar.Types
import Polar.Log

hello :: System
hello = defaultSystem "Hello"
    & startup  .~ startupF
    & shutdown .~ shutdownF
    & tick     .~ tickF

startupF :: Sys ()
startupF = logWrite WARNING "let's make some noise"

shutdownF :: Sys ()
shutdownF = logWrite WARNING "i don't hate you"

tickF :: Sys ()
tickF = do
    logWrite INFO "hello"
    exit
```

###### `Main.hs`
```haskell
import Polar.Run
import Hello

main :: IO ()
main = run $ defaultEngine
    & systems .~ [hello]
```

### Text Renderer

###### `TextRenderer.hs`

```haskell
module TextRenderer (textRenderer) where

import Data.Foldable (traverse_)

textRenderer :: System
textRenderer = defaultSystem "Text Renderer"
    & tick .~ tickF

tickF :: Sys ()
tickF = traverse_ f =<< retrieveAll Proxy
  where f s = logWrite INFO ("Saw " ++ s)
```

###### `Main.hs`
```haskell
import Polar.Run
import TextRenderer

main :: IO ()
main = run $ defaultEngine
    & systems .~ [textRenderer]
```
