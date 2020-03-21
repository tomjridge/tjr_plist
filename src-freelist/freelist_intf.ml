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


type 'a freelist = {
  transient          : 'a list;
  waiting            : ('a Event.event list);
  disk_thread_active : bool;
}
(** The freelist state, in addition to the plist.

transient: a non-persistent list of free elts that can be allocated

waiting: a list of threads waiting for a disk process to return

NOTE: we assume that the state is accessed via with_state, ie, only one thread at a time

 *)
