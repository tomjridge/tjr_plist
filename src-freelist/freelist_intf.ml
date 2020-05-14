(** Main interfaces used by the freelist *)


open Tjr_plist.Plist_intf


(** There are two versions of the freelist:
- For_blkids: to implement the "usual" blk freelist, managing on-disk blocks; the elts are actually blk_ids

- For_arbitrary_elts: which allocates and frees arbitrary elements; some other freelist of the first type provides the alloc and free blk functionality
*)
type ('elt,'blk_id,'t) version = 
  | For_blkids of { e2b:'elt -> 'blk_id; b2e: 'blk_id -> 'elt } 
  | For_arbitrary_elts of 
      { alloc: unit -> ('blk_id,'t)m; free: 'blk_id -> (unit,'t)m }

  (* write_freelist_roots : hd:'blk_id -> tl:'blk_id -> (unit,'t)m; *)


type ('a,'blk_id) fl_root_info = {
  hd: 'blk_id;
  tl: 'blk_id;
  blk_len: int;
  min_free: 'a option
}

type ('a,'blk_id,'t) fl_root_ops = {
  write_root : ('a,'blk_id) fl_root_info -> (unit,'t)m;
  sync       : unit -> (unit,'t)m;
}
(**
- write_root: we can recover the freelist providing we know the hd, tl and blk_len of the underlying plist
- sync: ensure the freelist roots are actually on disk; redundant if
   we assume the blk_dev_ops write direct to disk with no cache; this
   sync is invoked by freelist_ops.sync
*)



type sync_type = [ `Tl_only | `Tl_and_root_block ]
(**

`Tl: this just ensures that the tl block of the freelist is synced; it does not update the root block

`Tl_and_root_block: sync tl and global root block

*)

type fIXME

type ('blk_id,'t) freelist_ops = {
  alloc      : unit -> ('blk_id,'t)m;
  alloc_many : int -> (fIXME,'t)m;
  free       : 'blk_id -> (unit,'t)m;
  free_many  : fIXME -> (unit,'t)m;
  sync       : sync_type -> (unit,'t)m;
}
(** alloc_many: int is the number of blk_ids (although this sort-of
   forces us to unmarshal the whole block; perhaps prefer also storing
   the number of elements in the block, precisely for this reason FIXME

free_many: free an entire list of blk_ids, by appending to this
   freelist *)


type 'a min_free_ops = {
  min_free_alloc: 'a -> int -> 'a list * 'a
}
(** Working with a min_free element, we provide a method that takes
   the min_free element, and a count of the number of frees required,
   and returns a list of free elements (of the same length as
   requested) and the new min_free *)

type 'a freelist = {
  transient          : 'a list; 
  min_free           : ('a * 'a min_free_ops) option;
  
  waiting            : ('a event list);
  disk_thread_active : bool;
}
(** The freelist state, in addition to the plist.

- transient: a non-persistent list of free elts that can be allocated

- min_free: free elts are stored in transient, or in the on-disk plist;
usually we also have an "upper bound", beyond which we can still
allocate elts freely (an elt is either in transient, allocated to some
object, in the plist, or geq min_free)

- waiting: a list of threads waiting for a disk process to return

NOTE: we assume that the state is accessed via with_state, ie, only one thread at a time

*)


(** This type can be used to marshal the freelist root block *)
type ('a,'blk_id) fl_root_mshlr = 
  ('blk_id * 'blk_id * int * 'a option) bp_mshlr

(* assume monad ops etc given; assume blk_dev_ops given; assume
   fl_root_mshlr given *)
(* $(PIPE2SH("""sed -n '/type[ ].*freelist_factory/,/^>/p' >GEN.freelist_factory.ml_""")) *)
type ('a,'buf,'blk_id,'t) freelist_factory = <
  fl_root_mshlr : ('a,'blk_id) fl_root_mshlr;
  from_disk     : <
    version     :('a, 'blk_id, 't) version;
    fl_root_blk : 'blk_id
  > -> 
    < 
      freelist_ops  : ('blk_id,'t)freelist_ops; 
      (** the freelist operations! *)

      fl_root_ops   : ('a,'blk_id, 't) fl_root_ops; (** uses fl_root_mshlr *)
      with_freelist : ('a freelist,'t)with_state;
      freelist_ref  : 'a freelist ref;
      plist_ops     : ('a,'buf,'blk_id,'t)plist_ops;
      with_plist    : (('blk_id,'buf)plist,'t)with_state;
      plist_ref     : ('blk_id,'buf)plist ref;
    >
>
