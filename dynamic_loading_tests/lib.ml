let main_init () = print_string "initial\n"
let main_ref = ref main_init
let main () = (!main_ref) ()
let ch_main f = main_ref := f
