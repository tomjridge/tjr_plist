(** A persistent (on-disk) list. The freelist based on this is 
concurrent-safe (multiple writers are allowed, but only a single thread interacts with disk), but this code is not.

{%html:
<img width='100%' src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">
%}

Look at {!Plist_intf} for the main types. 

A persistent list is an on-disk list, where elements can be added at
the tail. The "origin block" stores the hd blkid, the tl blkid and the
length of the list. From this, the in-memory state of the list can be
reconstructed after a system crash. The hd is expected not to change
(much). The tl may change (as new elts get added and the tl block gets
full, the tl advances) but since we can follow links from the hd (or
the old tl) we don't have to modify the origin block when the tl
changes. Thus, we can add elements to the end of the list, and make
the changes persistent with a single sync.

We also have an additional operation of "adv_hd", which moves the hd
pointer to the next blk. This does require an update to the origin
block before the new hd is made visible to the rest of the system.

The adv_hd operation is used by the freelist. For the freelist, each
block on disk stores a list of (free) blkids. To avoid repeated disk
access, free blkids are typically transferred to memory as a batch
using a single blk read (via the adv_hd operation).
Sync behaviour: There are at least 2 choices (in addition to
respecting explicit syncs):

- sync on every add
- sync on moving to a new tl

The current implementation syncs when moving to a new tail, not on
every add. Thus, some elements that have been added, but not synced,
may be lost on a system crash. If needed, you can avoid this by
explicitly syncing after each add (with the commensurate loss of
performance). The sequence of operations is:

- Allocate new tail block and initialize it; sync 
- Update current tail block with next pointer to new tail; sync
- Update in-memory state so that the new tail becomes the "current" tail


*)

include Summary

module Plist_intf = Plist_intf

module Make_1 = Make_1

module Make_5 = Make_5

(* $(CONVENTION("Place common examples under eg Tjr_plist.pl_examples
   object; this includes std_ctxt-specific type instances eg
   origin_mshlr for blk_id = int ")) *)

let pl_examples = 
  let open Pl_type_abbrevs in
  let open (struct
    module S = struct 
      type nonrec buf = buf
      type nonrec blk_id = blk_id
      type nonrec blk = blk
      type nonrec t = t
    end
    module M5 = Make_5.Make(S)
    let plist_marshal_info: int plist_marshal_info = {
      elt_mshlr=mshlrs#for_int_option;
      blk_id_mshlr=mshlrs#for_blk_id_option;
      blk_to_buf=blk_to_buf;
      buf_to_blk=buf_to_blk;
    }
    let int_plist_factory = M5.plist_factory ~monad_ops ~buf_ops ~blk_ops 
        ~plist_marshal_info

  end)
  in
  let open (struct
    module S = struct 
      type nonrec buf = buf
      type nonrec blk_id = blk_id
      type nonrec blk = blk
      type nonrec t = t
    end
    module M5 = Make_5.Make(S)
    let plist_marshal_info: Shared_ctxt.r plist_marshal_info = {
      elt_mshlr=mshlrs#for_blk_id_option;
      blk_id_mshlr=mshlrs#for_blk_id_option;
      blk_to_buf=blk_to_buf;
      buf_to_blk=buf_to_blk;
    }
    let r_plist_factory = M5.plist_factory ~monad_ops ~buf_ops ~blk_ops 
        ~plist_marshal_info

  end)
  in
  (* specialize to Shared_ctxt.r for time being *)
  let origin_ops = 
    let open Shared_ctxt in
    fun 
      (* ~(r_mshlr:r bp_mshlr)  *)
      ~(blk_dev_ops :(_,_,_)blk_dev_ops) 
      ~(blk_id      : r) 
      ~(sync_blk_id : unit -> (unit,t)m)
      -> 
        let r_mshlr = bp_mshlrs#r_mshlr in
        let bp_mshlr = Plist_intf.Pl_origin.mshlr ~r_mshlr in
        let ba_mshlr = bp_mshlrs#ba_mshlr ~mshlr:bp_mshlr ~buf_sz:(Blk_sz.to_int blk_sz) in
        let module M = (val ba_mshlr) in
        let read = fun () -> blk_dev_ops.read ~blk_id >>= fun blk -> 
          return (M.unmarshal blk)
        in
        let write = fun t ->
          blk_dev_ops.write ~blk_id ~blk:(M.marshal t) 
        in
        let set_and_sync = fun t ->
          blk_dev_ops.write ~blk_id ~blk:(M.marshal t) >>= fun () -> 
          sync_blk_id ()
        in
        let obj = 
          object
            method read=read
            method write=write
            method set_and_sync=set_and_sync
          end
        in
        obj
  in
  object 
    method for_int : int plist_factory = int_plist_factory      
    method for_blk_id : Shared_ctxt.r plist_factory = r_plist_factory
    method origin_factory:(_,_,_)Plist_intf.origin_factory = object
      method monad_ops=monad_ops
      method with_=
        fun ~blk_dev_ops ~blk_id ~sync_blk_id -> 
        let x = origin_ops ~blk_dev_ops ~blk_id ~sync_blk_id in
        (x :> <set_and_sync: _ Plist_intf.Pl_origin.t -> (unit,'t)m>)
    end
  end

let _ = pl_examples

(** {[
< int_plist_factory : int Tjr_plist__.Pl_type_abbrevs.plist_factory;
  origin_factory : (Tjr_fs_shared.Shared_ctxt.r,
                    Tjr_fs_shared.Pvt_bin_prot_marshalling.ba_buf,
                    Tjr_monad.With_lwt.lwt)
                   Plist_intf.origin_factory >
]} *)


(** NOTE hidden doc for module [Pl_type_abbrevs] *)
(* $(CONVENTION("Name a module like pl_type_abbrevs for local type
   abbrevs assuming lwt etc")) *)

(**/**)
module Pl_type_abbrevs = Pl_type_abbrevs
(**/**)

