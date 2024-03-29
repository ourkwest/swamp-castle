# Cakewalk (working title)

Prototyping a board game.

## Setup

You can run this locally using the `leiningen` build tool.

Alternatively it is deployed here: http://peterwestmacott.github.io/games/domination/index.html

To get an interactive development environment run:

    lein figwheel

and open your browser at [localhost:3449](http://localhost:3449/).
This will auto compile and send all changes to the browser without the
need to reload. After the compilation process is complete, you will
get a Browser Connected REPL. An easy way to try it is:

    (js/alert "Am I connected?")

and you should see an alert in the browser window.

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL. 

## Copyright Notice

Copyright © 2017-2022 Rachel Westmacott
