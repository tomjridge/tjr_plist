

type ('a,'blk_id,'buf) plist = {
  hd          :'blk_id;
  tl          :'blk_id;
  tl_contents :'a list; (** needed? *) 
  tl_buffer   :'buf*int; (** including nxt pointer *)
  tl_nxt      :'blk_id option; (** store in first 8 bytes *)
  tl_dirty    :bool; (** may not have been written to disk *)
}


type ('a,'buf,'blk_id,'blk,'t) plist_ops = {
  add       : ?force_nxt:[`Before | `After | `False ] -> nxt_blk_id:'blk_id -> elt:'a -> ('blk_id option,'t) m;
  sync_tl   :unit -> (unit,'t)m; 

  empty     :'blk_id -> (('a,'blk_id,'buf)plist,'t)m;

  unmarshal :'blk->'a list * 'blk_id option; (** internal *)
  marshal   :'a list * 'blk_id option->'blk; (** internal *)  

  (* empty_node_as_blk:'blk;  *)
  (* Used to initialize the nxt block *)
  (* initialize_blk: 'blk_id -> 'a list -> (unit,'t)m; *)
}
(** add : The blk_id is returned if it is not used; another variant
   assumes an alloc function. We don't assume the nxt_blk is clean -
   we may write a single elt into the new blk then sync that before
   updating the nxt pointer.

   The optional force_nxt flag forces the move of tl to nxt_blk,
   either before or after attempting to write the elt (if the elt
   doesn't fit, we are forced to move anyway).

sync_tl : we automatically sync before moving to a new tl

empty : create an empty list in a given blk_id

*)

