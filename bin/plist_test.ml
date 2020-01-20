open Plist_examples

module T = Test()

let _ = 
  Lwt_main.run (T.main () |> Tjr_monad.With_lwt.to_lwt)
