#!/usr/bin/env bash

rm -rf elm-stuff/build-artifacts
rm -rf test/elm-stuff/build-artifacts

test/run.sh
if [ $? -ne 0 ]; then exit 1; fi;

elm make --yes --warn src/Collision.elm
