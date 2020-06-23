(** Freelist example 2.

This example uses the factory interface.
*)

open Shared_ctxt
open Freelist_intf
open Tjr_monad.With_lwt

(** Run_example: We open a freelist on a file, allocate some blk_ids,
   close, open, verify the state is consistent.  *)
let run_example ~params () = 
  let module A = struct

    let fn = params#filename

    (* let file_ops = lwt_file_ops *)
      
    let run_a () = 
      blk_devs#with_ba_buf#from_filename ~fn ~create:true ~init:true >>= fun bd ->

      let module B = struct
        
        let blk_dev_ops = bd#blk_dev_ops

        let fact = fl_examples#freelist_factory
                            
        let sync_blk_dev = bd#sync

        (* b0 holds the fl origin *)
        let b0 = B.of_int 0

        (* b1 is the start of the plist *)
        let b1 = B.of_int 1        
        
        (* b2 is first free block *)
        let b2 = B.of_int 2

        (* in-memory state *)
        let empty_freelist = fl_examples#empty_freelist ~min_free:(Some b2)

        let origin_ops = fact#origin_ops 
            ~blk_dev_ops ~origin_blkid:b0 ~sync_origin:sync_blk_dev

        let fact' = fact#with_
            ~blk_dev_ops ~sync_blk_dev ~origin_ops ~params:(params :> Freelist_intf.params)
        
        let run_b () = 
          (* we need to initialize b0 *)
          let origin = Fl_origin.{hd=b1;tl=b1;blk_len=1;min_free=Some b2} in
          origin_ops.write origin >>= fun () -> 

          (* and b1 *)
          pl_examples#for_blk_id#with_blk_dev_ops ~blk_dev_ops ~sync:sync_blk_dev |> fun x ->
          x#init#mk_empty b1 >>= fun plist ->

          (* and get the freelist ops *)
          x#with_ref plist |> fun x2 ->
          x#with_state (x2#with_plist) |> fun plist_ops ->
          fact'#with_plist_ops plist_ops |> fun x3 -> 
          x3#with_ref empty_freelist |> fun x4 -> 
          x4#freelist_ops |> fun freelist_ops -> 
          
          (* then run some operations *)
          let last = ref 0 in
          0 |> iter_k (fun ~k n ->
              match n >= 10_000 with
              | true -> return ()
              | false -> 
                freelist_ops.alloc () >>= fun i ->
                let i = B.to_int i in
                Printf.printf "Allocated blk_id: %d\n%!" i;
                last:=i;
                k (n+1))
          >>= fun () ->
          bd#close () >>= fun () ->
          Printf.printf "NOTE that the first allocated blk_id is 2, so \
                         after 10k allocs, we expect to be at blkid \
                         10_001\n%!";
          assert(!last = 10_001);
          Printf.printf "Finished\n%!";
          return ()
      end
      in
      B.run_b ()
  end
  in 
  A.run_a ()
        
