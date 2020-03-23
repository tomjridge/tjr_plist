(** Test freelist functionality *)
open Tjr_plist_freelist.Freelist_example

let _ = 
  let open (Run_example()) in
  Lwt_main.run (main |> Tjr_monad.With_lwt.to_lwt)
