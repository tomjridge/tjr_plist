(** Main interfaces used by the freelist *)

type ('blk_id,'t) root_block_ops = {
  write_freelist_roots : hd:'blk_id -> tl:'blk_id -> (unit,'t)m;
  sync                 : unit -> (unit,'t)m;
}


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
object, in the plist, or geq min_free

- waiting: a list of threads waiting for a disk process to return

NOTE: we assume that the state is accessed via with_state, ie, only one thread at a time

 *)

module Std_types = struct
  include Tjr_plist.Plist_intf.Std_types
  type nonrec root_block_ops = (blk_id,t)root_block_ops
  type nonrec 'a freelist_ops = ('a,t)freelist_ops
  type nonrec 'a with_freelist=('a freelist,t)with_state
end
