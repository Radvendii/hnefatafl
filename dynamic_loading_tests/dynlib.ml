Lib.change
  (
    module struct
      let main = (fun _ -> print_string "changed\n")
    end
  )
