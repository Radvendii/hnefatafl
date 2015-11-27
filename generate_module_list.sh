entry(){
    local f=$1
    local m=${f%.ml}
    local n=${m#GUI_}
    echo "\"$n\", (module $m.GUI : GUI)"
}

cp GUI_list_template.ml GUI_list.ml
printf "let gui_list =\n  [ " >> GUI_list.ml

first=1
for f in $(ls | grep GUI_)
do
    if [ "${f}" = "GUI_list.ml" ]
    then
        break
    fi
    if [ "${f}" = "GUI_list_template.ml" ]
    then
        break
    fi
    if [ $first = 0 ]
    then
        printf "  ; " >> GUI_list.ml
    fi
    printf "$(entry $f)\n" >> GUI_list.ml
    first=0
done
printf "  ]" >> GUI_list.ml
