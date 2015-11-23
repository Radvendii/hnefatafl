module type LibType = sig
  val main : 'a -> unit
end

(* module Library = struct *)
(*   let main = ref(fun () -> print_string "initial\n") *)
(* end *)

module Dummy : LibType = struct
  let main _ = ()
end

let modref = ref (module Dummy : LibType)

let change (fcmod) =
  modref := fcmod

module Library : LibType = struct
  let main a =
    let modval = !modref in
    let module Mod = (val modval : LibType) in
    (Mod.main) a
end
