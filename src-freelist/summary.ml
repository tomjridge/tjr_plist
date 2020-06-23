(** Summary of main types *)

(**

{[
  type ('a,'blk_id) fl_origin = {
    hd: 'blk_id;
    tl: 'blk_id;
    blk_len: int;
    min_free: 'a option
  }[@@deriving bin_io]

  (* fl_origin ops *)
  type ('a,'blk_id,'t) ops = {
    read  : unit -> (('a,'blk_id)t,'t)m;
    write : ('a,'blk_id)t -> (unit,'t)m;
    sync  : unit -> (unit,'t)m;
  }

(** In-memory state for the freelist *)
type 'a freelist = {
  transient          : 'a list; 
  min_free           : ('a * 'a min_free_ops) option;
  
  waiting            : ('a event list);
  disk_thread_active : bool;
}

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

          with_ref : 'a freelist -> 
            < freelist_ops: ('a,'t)freelist_ops;
              freelist_ref: 'a freelist ref;
            >
        (** use an imperative ref to hold the state *)
        >
    >
>

type ('blk_id,'t) freelist_ops = {
  alloc      : unit -> ('blk_id,'t)m;
  alloc_many : int -> (fIXME,'t)m;
  free       : 'blk_id -> (unit,'t)m;
  free_many  : fIXME -> (unit,'t)m;
  sync       : unit -> (unit,'t)m;
  (** NOTE the freelist already ensures it is crash safe; this sync is
     really for tidy shutdown *)
}

type ('elt,'blk_id,'t) version = 
  | For_blkids of ('elt,'blk_id) for_blk_ids
  | For_arbitrary_elts of 
      { alloc: unit -> ('blk_id,'t)m; free: 'blk_id -> (unit,'t)m }
      (** For arbitrary elts, we need a way to allocate and free blocks *)
and ('elt,'blk_id) for_blk_ids = 
  { e2b:'elt -> 'blk_id; b2e: 'blk_id -> 'elt } 


]}

*)

