(** Summary of main types *)

(**

{[
  type ('a,'blk_id) fl_origin = {
    hd: 'blk_id;
    tl: 'blk_id;
    blk_len: int;
    min_free: 'a option
  }

  (* fl_origin ops *)
  type ('a,'blk_id,'t) ops = {
    read  : unit -> (('a,'blk_id)t,'t)m;
    write : ('a,'blk_id)t -> (unit,'t)m;
    sync  : unit -> (unit,'t)m;
  }


type ('a,'buf,'blk_id,'t) freelist_factory = <
  version       : ('a, 'blk_id) for_blk_ids; 
  (** NOTE specialized to 'a =iso= 'blk_id *)

  origin_ops: 
    blk_dev_ops :('blk_id,'buf,'t)blk_dev_ops -> 
    blk_id      :'blk_id -> 
    sync_blk    :(unit -> (unit,'t)m) ->
    ('a,'blk_id,'t) Fl_origin.ops;
    

  with_: 
    < blk_dev_ops   : ('blk_id,'buf,'t)blk_dev_ops;
      sync_blk_dev  : (unit -> (unit,'t)m);
      origin_blkid  : 'blk_id; 
    >
    -> <
      with_state : 
        ('a,'t)with_state -> ('a,'t)freelist_ops;

      with_ref   : unit -> ('a,'t)freelist_ops;
      (** use an imperative ref to hold the state *)
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
and ('elt,'blk_id) for_blk_ids = 
  { e2b:'elt -> 'blk_id; b2e: 'blk_id -> 'elt } 


]}

*)

