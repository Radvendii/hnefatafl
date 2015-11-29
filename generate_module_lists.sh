#!/usr/bin/env bash
gui_entry(){
    local f=$1
    local n=${f%.ml}
    local m=${n^}
    echo "\"$n\", (module $m.GUI : GUI)"
}
mode_entry(){
    local f=$1
    local n=${f%.ml}
    local m=${n^}
    echo "\"$n\", (module $m.Mode : Game_mode)"
}

cp GUI_list_template.ml GUI_list.ml
printf "\nlet gui_list =\n  [ " >> GUI_list.ml

first=1
for f in $(ls guis/)
do
    if [ $first = 0 ]
    then
        printf "  ; " >> GUI_list.ml
    fi
    printf "$(gui_entry $f)\n" >> GUI_list.ml
    first=0
done
printf "  ]" >> GUI_list.ml


cp MODE_list_template.ml MODE_list.ml
printf "\nlet mode_list =\n  [ " >> MODE_list.ml
first=1
for f in $(ls game_modes/)
do
    if [ "${f}" = "berserk.ml" ]
    then
        continue
    fi
    if [ $first = 0 ]
    then
        printf "  ; " >> MODE_list.ml
    fi
    printf "$(mode_entry $f)\n" >> MODE_list.ml
    first=0
done
printf "  ]" >> MODE_list.ml
