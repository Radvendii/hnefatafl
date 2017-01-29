# Hnefatafl
Final Project for CS3110: Coding Hnefatafl

# Setup
Designed to be run on the CS3110 Virtual Machine: http://www.cs.cornell.edu/courses/cs3110/2015fa/vm.html

Run setup.sh in the main folder to install the needed packages.

Compile and Run with the following commands:

    cs3110 compile main.ml
    cs3110 run main.ml

# Adding GUIs
There are a couple of options of interfaces in this project. You can add your own by creating a .ml file in the guis/ subdirectory that exhibits a GUI module with the GUI module type found in GUI.mli. To name your GUI, put a comment at the top that appears exactly as follows:

    (*
       NAME: <the gui's name>
    *)


# Adding Game Modes
Many variants of Hnefatafl exist. A few of them can be found in this project. To add your own, add a .ml file in the game_modes/ subdirectory that exhibits a Mode module with the Mode module type found in game_mode.mli. names folow the same naming convention

# Screenshots
The starting layout for the default game mode in the 3D graphics GUI
![](https://raw.githubusercontent.com/Radvendii/hnefatafl/master/imgs/1.png)

Hovering the cursor over a piece:
![The cursor is a blue light under the piece](https://raw.githubusercontent.com/Radvendii/hnefatafl/master/imgs/2.png)

Piece selected:
![The piece turns translucent](https://raw.githubusercontent.com/Radvendii/hnefatafl/master/imgs/3.png)

Hovering over a valid move:
![A translucent copy of the piece shaded green follows the cursor](https://raw.githubusercontent.com/Radvendii/hnefatafl/master/imgs/4.png)

Hovering over an invalid move:
![The copy of the piece following the cursor turns red](https://raw.githubusercontent.com/Radvendii/hnefatafl/master/imgs/5.png)

Move made! Background changes to white to indicat the current turn.
![](https://raw.githubusercontent.com/Radvendii/hnefatafl/master/imgs/6.png)
