open Plist_examples

type a = 
  | A1_ocaml_marshal
  | A2_binprot

let map_a = function
  | 1 -> A1_ocaml_marshal
  | 2 -> A2_binprot
  | _ -> failwith __LOC__

let now = Tjr_profile.now

let _ = 
  let p i = 
    i |> map_a |> function
    | A1_ocaml_marshal -> 
      let module X = Blk_as_bytes_buf_as_bytes.Test() in
      X.main ()
    | A2_binprot -> 
      let module X = Blk_as_ba_buf_as_ba.Test() in
      X.main ()
  in
  let task = 
    return () >>= fun () ->
    let t1 = now() in
    p 1 >>= fun () ->
    let t2 = now() in
    p 2 >>= fun () -> 
    let t3 = now() in
    Printf.printf "Timings: p1:%d p2:%d\n" (t2-t1) (t3-t2);
    return ()
  in    
  Lwt_main.run ( task |> Tjr_monad.With_lwt.to_lwt)
