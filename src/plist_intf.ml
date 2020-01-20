

type ('a,'blk_id,'buf) plist = {
  hd             : 'blk_id;
  tl             : 'blk_id;
  buffer      : 'buf;
  off         : int;
  nxt_is_none : bool; 
  dirty       : bool; (** may not have been written to disk *)
}
(** This is an internal implementation type.
NOTE modifications take place in the tl block *)


(** Internal operations, for debugging *)
type ('a,'blk_id,'blk) plist_marshal_ops = {  
  unmarshal : 'blk->'a list * 'blk_id option; 
  marshal   : 'a list * 'blk_id option->'blk; 
}

(** Operations which don't require the plist state; typically
   initialization and debugging *)
type ('a,'buf,'blk_id,'blk,'t) plist_extra_ops = {  
  create_plist : 'blk_id -> (('a,'blk_id,'buf)plist,'t)m;
  read_plist   : 'blk_id -> ( ('a list * 'blk_id option) list, 't) m;
}

(** plist operations which require the plist state from the monad *)
type ('a,'buf (* FIXME *),'blk_id,'blk,'t) plist_ops = {
  add  : nxt:'blk_id -> elt:'a -> ('blk_id option,'t) m;
  sync : unit -> (unit,'t)m; 
}
(** add : The blk_id is returned if it is not used; another variant
   assumes an alloc function. We don't assume the nxt_blk is clean -
   we may write a single elt into the new blk then sync that before
   updating the nxt pointer.

sync : we automatically sync before moving to a new tl; this is for partial blocks which we need to sync
*)

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


(** The result of making the plist *)
module Ret_ = struct
  type ('a,'blk_id,'blk,'buf,'t) ret_ = {
    m_u_ops      : ('a, 'blk_id, 'blk) plist_marshal_ops;
    extra_ops    : ('a,'buf,'blk_id,'blk,'t) plist_extra_ops;
    plist_ops    : (('a, 'blk_id, 'buf) plist, 't) with_state ->
      ('a,'buf,'blk_id,'blk,'t) plist_ops
  }
  (** What we return from making the plist; note that the [plist_ops]
      require a [with_state] *)
end
include Ret_
