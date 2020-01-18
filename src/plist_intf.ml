type ('a,'blk_id,'buf) plist = {
  hd             : 'blk_id;
  tl             : 'blk_id;
  buffer      : 'buf;
  off         : int;
  nxt_is_none : bool; 
  dirty       : bool; (** may not have been written to disk *)
}
(** NOTE modifications take place in the tl block *)

  (* tl_contents : 'a list; (\** needed? *\)  *)

(* FIXME we need to ensure buffer and tl_nxt_nil are coherent; we need
   to identify tl_nxt=None without unmarshalling from the buffer *)

type ('a,'blk_id,'blk) plist_marshal_ops = {  
  unmarshal : 'blk->'a list * 'blk_id option; (** internal *)
  marshal   : 'a list * 'blk_id option->'blk; (** internal *)  
}

type ('a,'buf,'blk_id,'blk,'t) plist_init_ops = {  
  create_plist : 'blk_id -> (('a,'blk_id,'buf)plist,'t)m;
}

type ('a,'buf,'blk_id,'blk,'t) plist_ops = {  
  add  : nxt:'blk_id -> elt:'a -> ('blk_id option,'t) m;
  sync : unit -> (unit,'t)m; 
}
(** add : The blk_id is returned if it is not used; another variant
   assumes an alloc function. We don't assume the nxt_blk is clean -
   we may write a single elt into the new blk then sync that before
   updating the nxt pointer.

   The optional force_nxt flag forces the move of tl to nxt_blk,
   either before or after attempting to write the elt (if the elt
   doesn't fit, we are forced to move anyway).

sync_tl : we automatically sync before moving to a new tl
*)



  (* empty_node_as_blk:'blk;  *)
  (* Used to initialize the nxt block *)
  (* initialize_blk: 'blk_id -> 'a list -> (unit,'t)m; *)


module Plist_marshal_info = struct
  type ('a,'blk_id,'blk,'buf) plist_marshal_info = {
    max_elt_sz    :int;
    max_blk_id_sz :int;
    m_elt         : 'a option -> 'buf*int -> 'buf*int;
    u_elt         : 'buf -> int -> 'a option * int;
    m_blk_id      : 'blk_id option -> 'buf*int -> 'buf*int;
    u_blk_id      : 'buf -> int -> 'blk_id option*int;
    blk_to_buf: 'blk -> 'buf;
    buf_to_blk: 'buf -> 'blk  
  }
(**

   We write an elt e as "marshal(Some e)" and the end-of-list marker as
   "marshal(None)"

   - max_elt_sz: max size of a marshalled elt option

   - max_blk_id_sz: max size of a marshalled blk_id option

*)
end
include Plist_marshal_info
