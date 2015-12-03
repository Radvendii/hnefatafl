#!/usr/bin/env bash
sudo apt-get update
sudo apt-get install libffi-dev
sudo apt-get install sdl1.2-dev
opam install termbox
opam install ocamlsdl
mkdir -p ~/.blender/scripts
cp ./blender_export.py ~/.blender/scripts
