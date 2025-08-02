# Cakewalk

Prototyping a board game.

## What is this repo?

A collection of bits of code that have been used to prototype the boardgame 'Cakewalk'. Some are in an advanced state of
decomposition.

What may still be usable is the code to generate the artwork for printing copies of the game.

## How do I use it?

1. Install [leiningen](https://github.com/technomancy/leiningen/tree/master) (yes, it's an old project)
2. Run the following command:

`lein repl`

3. When the REPL is running:

```
(require '[board-game.images.core :as core])
(core/create-all-images)
(exit)
```

## Copyright Notice

Copyright © 2017-2025 Rachel K. Westmacott
