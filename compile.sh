#!/usr/bin/env bash

elm-test

if [ $? -ne 0 ]; then exit 1; fi;

elm make --yes --warn src/Collision.elm
elm make --docs=documentation.json

cd example
elm make --yes --warn src/Main.elm
