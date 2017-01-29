# hnefatafl
Final Project for CS3110: Coding Hnefatafl

# Setup
Designed to be run on the CS3110 Virtual Machine: http://www.cs.cornell.edu/courses/cs3110/2016fa/install.html
Run setup.sh in the main folder to install the needed packages.

# Adding GUIs
There are a couple of options of interfaces in this project. You can add your own by creating a .ml file in the guis/ subdirectory that exhibits a GUI module with the GUI module type found in GUI.mli. To name your GUI, put a comment at the top that appears exactly as follows:
<code>
(*
   NAME: <the gui's name>
*)
</code>

# Adding Game Modes
Many variants of Hnefatafl exist. A few of them can be found in this project. To add your own, add a .ml file in the game_modes/ subdirectory that exhibits a Mode module with the Mode module type found in game_mode.mli. names folow the same naming convention
