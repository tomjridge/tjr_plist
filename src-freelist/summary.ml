(** Summary of main types *)

(**

{[
  val make: 
    monad_ops     :(t monad_ops) ->     
    event_ops     :t event_ops ->
    async         :((unit -> (unit, t) m) -> (unit, t) m) ->
    plist_ops     :(elt, buf, blk_id, t) plist_ops ->
    with_freelist :(elt freelist, t) with_state ->
    root_block    :(elt, blk_id, t) fl_root_ops ->
    version       :(elt,blk_id,t)version -> 
    (elt, t) freelist_ops

type ('a,'blk_id) fl_root_info = {
  hd: 'blk_id;
  tl: 'blk_id;
  blk_len: int;
  min_free: 'a option
}

type ('a,'blk_id) fl_root_mshlr = ('a,'blk_id) fl_root_info bp_mshlr
(** This type can be used to marshal the freelist root block; NOTE
   this is effectively specialized to 'blk = 'buf = bigarray *)


type ('a,'blk_id,'t) fl_root_ops = {
  read_root  : unit -> ( ('a,'blk_id)fl_root_info, 't)m;
  write_root : ('a,'blk_id) fl_root_info -> (unit,'t)m;
  sync       : unit -> (unit,'t)m;
}

type ('a,'buf,'blk_id,'t) freelist_factory = <
  version       : ('a, 'blk_id) for_blk_ids; 
  (** NOTE specialized to 'a =iso= 'blk_id *)

  read_root: 
    blk_dev_ops :('blk_id,'buf,'t)blk_dev_ops -> 
    blk_id      :'blk_id -> 
    ( ('a,'blk_id)fl_root_info, 't)m;
  
  write_root:
    blk_dev_ops :('blk_id,'buf,'t)blk_dev_ops -> 
    blk_id      :'blk_id -> 
    ('a,'blk_id)fl_root_info -> 
    (unit,'t)m;

  with_: 
    < blk_dev_ops   : ('blk_id,'buf,'t)blk_dev_ops;
      plist_ops     :('a,'buf,'blk_id,'t) plist_ops;
      with_freelist : ('a,'t)with_state;
      fl_root_blk   : 'blk_id; 
    >
    -> ('a,'t)freelist_ops;

  from_disk: 
    < blk_dev_ops: ('blk_id,'buf,'t)blk_dev_ops;      
      fl_root_blk: 'blk_id -> (('a,'t)freelist_ops,'t)m
    >
    -> ('a,'t)freelist_ops;

>

type ('elt,'blk_id,'t) version = 
  | For_blkids of ('elt,'blk_id) for_blk_ids
  | For_arbitrary_elts of 
      { alloc: unit -> ('blk_id,'t)m; free: 'blk_id -> (unit,'t)m }
and ('elt,'blk_id) for_blk_ids = 
  { e2b:'elt -> 'blk_id; b2e: 'blk_id -> 'elt } 


]}

*)
