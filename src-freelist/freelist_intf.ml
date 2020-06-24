(** Main interfaces used by the freelist *)


open Tjr_plist.Plist_intf


(** 

There are two versions of the freelist:

- For_blkids: to implement the "usual" blk freelist, managing on-disk
  blocks; the elts are actually blk_ids

- For_arbitrary_elts: which allocates and frees arbitrary elements;
  some other freelist of the first type provides the alloc and free
  blk functionality

*)
(* $(PIPE2SH("""sed -n '/type[ ].*version/,/^$/p' >GEN.version.ml_""")) *)
type ('elt,'blk_id,'t) version = 
  | For_blkids of ('elt,'blk_id) for_blk_ids
  | For_arbitrary_elts of 
      { alloc: unit -> ('blk_id,'t)m; free: 'blk_id -> (unit,'t)m }
      (** For arbitrary elts, we need a way to allocate and free blocks *)
and ('elt,'blk_id) for_blk_ids = 
  { e2b:'elt -> 'blk_id; b2e: 'blk_id -> 'elt } 



module Fl_origin = struct
  open Bin_prot.Std
  (* $(PIPE2SH("""sed -n '/type[ ][^=]*fl_origin/,/}/p' >GEN.fl_origin.ml_""")) *)
  type ('a,'blk_id) fl_origin = {
    hd: 'blk_id;
    tl: 'blk_id;
    blk_len: int;
    min_free: 'a option
  }[@@deriving bin_io]

  type ('a,'blk_id) t = ('a,'blk_id) fl_origin[@@deriving bin_io]

  (* $(PIPE2SH("""sed -n '/fl_origin[ ]ops/,/^  }/p' >GEN.fl_origin_ops.ml_""")) *)
  (* fl_origin ops *)
  type ('a,'blk_id,'t) ops = {
    read  : unit -> (('a,'blk_id)t,'t)m;
    write : ('a,'blk_id)t -> (unit,'t)m;
    sync  : unit -> (unit,'t)m;
  }

    (* write          : blk_id:'blk_id -> origin:('a,'blk_id)t -> (unit,'t)m; *)
end


type fIXME

(* $(PIPE2SH("""sed -n '/type[ ].*freelist_ops/,/^}$/p' >GEN.freelist_ops.ml_""")) *)
type ('blk_id,'t) freelist_ops = {
  alloc      : unit -> ('blk_id,'t)m;
  alloc_many : int -> (fIXME,'t)m;
  free       : 'blk_id -> (unit,'t)m;
  free_many  : fIXME -> (unit,'t)m;
  sync       : unit -> (unit,'t)m;
  (** NOTE the freelist already ensures it is crash safe; this sync is
     really for tidy shutdown *)
}
(** alloc_many: int is the number of blk_ids although this sort-of
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


(* $(PIPE2SH("""sed -n '/In-memory[ ]state for the freelist /,/^}$/p' >GEN.freelist.ml_""")) *)
(** In-memory state for the freelist *)
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

NOTE: we assume that the state is accessed via with_state (ie with a
mutex), so only one thread at a time modifies state

*)

let empty_freelist ~min_free = {
  transient=[];
  min_free;
  waiting=[];
  disk_thread_active=false
}

let _ = empty_freelist


type params = <
  tr_upper:int;
  tr_lower:int;
  min_free_alloc_size:int
>
      
(* NOTE 'a is 'blk_id when working with the standard freelist *)

(* FIXME implement this freelist_factory for standard types *)

(* $(PIPE2SH("""sed -n '/type[ ].*freelist_factory/,/^>/p' >GEN.freelist_factory.ml_""")) *)
type ('a,'buf,'blk_id,'t) freelist_factory = <
  version       : ('a, 'blk_id) for_blk_ids; 
  (** NOTE this is for freelist only, not arbitrary elts *)

  empty_freelist : min_free:('a * 'a min_free_ops) option -> 'a freelist;
  (** [min_free] depends on the nature of 'a; for 'a = blk_id, we can
     use the origin blk_id and incr to implement min_free *)

  origin_ops: 
    blk_dev_ops  : ('blk_id,'buf,'t)blk_dev_ops -> 
    origin_blkid : 'blk_id -> 
    sync_origin  : (unit -> (unit,'t)m) ->
    ('a,'blk_id,'t) Fl_origin.ops;
    

  with_: 
    blk_dev_ops  : ('blk_id,'buf,'t)blk_dev_ops ->
    sync_blk_dev : (unit -> (unit,'t)m) -> 
    origin_ops   : ('a,'blk_id,'t) Fl_origin.ops -> 
    params       : params ->
    < 
      read_origin : unit -> (<
          fl_origin: ('a,'blk_id)Fl_origin.t;
          pl_origin: 'a Pl_origin.t
        >,'t)m;

      plist_ops : 'a Pl_origin.t -> (('a,'buf,'blk_id,'t) plist_ops,'t)m;

      with_plist_ops : ('a,'buf,'blk_id,'t) plist_ops -> 
        <
          with_state : 
            ('a freelist,'t)with_state -> ('a,'t)freelist_ops;

          with_locked_ref : 'a freelist -> 
            < freelist_ops: ('a,'t)freelist_ops;
              freelist_ref: 'a freelist ref;
            >
        (** use an imperative ref to hold the state; lock for concurrency *)
        >
    >
>


