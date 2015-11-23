Lib.change
  (
    module struct
      let main = (fun () -> print_string "changed\n")
    end
  )
