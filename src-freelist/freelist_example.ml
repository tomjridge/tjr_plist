(** Freelist example.

Various examples (fixed elt type). Also some code to exercise the
   functionality.

*)
open Sh_std_ctxt
open Freelist_intf

let int_plist_ops : 
  int Make_4.plist_marshal_ops *
  [> `K1 of
       blk_dev_ops:blk_dev_ops' ->
       int Make_4.plist_extra_ops *
       [> `K2 of
            with_state:(Make_4.plist, Make_4.t) with_state ->
            int Make_4.plist_ops ] ]
  = Plist_factory.int_plist_ops


(** Run_example: We open a freelist on a file, allocate some blk_ids,
   close, open, verify the state is consistent.  *)
module Run_example() = struct

  open Tjr_monad.With_lwt

  let fn = "freelist_example.store"

  let file_ops = lwt_file_ops

  let main : (unit,t)m = 

    (* file descriptor, blk_dev_ops *)
    file_ops.open_ ~fn ~create:true ~init:true >>= fun fd ->
    let blk_dev_ops = Blk_dev_factory.make_5 fd in

    (* plist *)
    let plist_extra_ops,rest = 
      snd int_plist_ops |> fun (`K1 rest) ->
      rest ~blk_dev_ops |> fun (plist_extra_ops,`K2 rest) ->
      plist_extra_ops,rest
    in
    let b1 = B.of_int 1 in
    (* NOTE create_plist writes the empty list to blk *)
    plist_extra_ops.create_plist b1 >>= fun pl ->
    let pl_ref = ref pl in
    let with_state = with_ref pl_ref in
    let plist_ops = rest ~with_state in

    (* freelist *)
    let root_block = {
      write_freelist_roots=(fun ~hd ~tl -> 
          (* FIXME should really write into blk 0 *)
          Printf.printf "call to write_freelist_roots\n%!";
          return ());
      sync=(fun () -> 
          Printf.printf "call to sync\n%!";
          return ())
    }
    in
    (* NOTE this actually uses ints rather than blk_ids, since we are
       using int_plist_ops FIXME use blkid_plist_ops *)
    let version = Freelist_make.For_blkids { 
        e2b=(fun x -> B.of_int x); 
        b2e=(fun x -> B.to_int x) } 
    in
    (* start off with some free elts in transient *)
    let min_free_alloc elt n = (List_.from_upto elt (elt+n), elt+n) in
    let min_free = Some (6,{ min_free_alloc }) in
    let fl_ref = ref { 
        transient=[2;3;4;5]; min_free; 
        waiting=[]; disk_thread_active=false } 
    in
    (* let with_freelist = with_ref fl_ref in *)
    (* for the freelist, we need to ensure that it is actually locked *)
    let with_freelist = with_locked_ref fl_ref in
    let freelist_ops = Make_2.(Freelist_make.make (object
                                 method async=async
                                 method event_ops=event_ops
                                 method monad_ops=monad_ops
                                 method plist=plist_ops 
                                 method root_block=root_block
                                 method version=version
                                 method with_freelist=with_freelist
                               end))
    in
    
    (* at last, can actually start allocating *)

    let last = ref 0 in
    0 |> iter_k (fun ~k n ->
        match n >= 10_000 with
        | true -> return ()
        | false -> 
          freelist_ops.alloc () >>= fun i ->
          Printf.printf "Allocated blk_id: %d\n%!" i;
          last:=i;
          k (n+1))
    >>= fun () ->
    file_ops.close fd >>= fun () ->
    (* FIXME nondet bug: sometimes we finish at 10001, sometimes at 10002 *)
    Printf.printf "NOTE that the first allocated blk_id is 2, so after 10k allocs, we expect to be at blkid 10_001\n%!";
    assert(!last = 10_001);
    Printf.printf "Finished\n%!";
    return ()
end

    
    (* let min_free_blk_ref = ref (B.of_int 2) in *)
    (* let blk_alloc = make_blk_allocator min_free_blk_ref in *)
