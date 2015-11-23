module type LibType = sig
  val main : unit -> unit
end

module Library = struct
  let main = ref(fun () -> print_string "initial\n")
end

let change (fcmod) =
  let module L = (val fcmod : LibType) in
  Library.main := L.main

let main () = !(Library.main) ()
