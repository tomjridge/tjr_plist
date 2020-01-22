

type ('a,'blk_id,'buf) plist = {
  hd      : 'blk_id;
  tl      : 'blk_id;
  buffer  : 'buf;
  off     : int;
  blk_len : int;
  dirty   : bool; (** may not have been written to disk *)
}
(** This is an internal implementation type.
NOTE modifications take place in the tl block *)

  (* nxt_is_none : bool;  *)


(** Internal operations, for debugging *)
type ('a,'blk_id,'blk) plist_marshal_ops = {  
  unmarshal : 'blk->'a list * 'blk_id option; 
  marshal   : 'a list * 'blk_id option->'blk; 
}

(** Operations which don't require the plist state; typically
   initialization and debugging *)
type ('a,'buf,'blk_id,'t) plist_extra_ops = {  
  create_plist : 'blk_id -> (('a,'blk_id,'buf)plist,'t)m;
  read_plist   : 'blk_id -> ( ('a list * 'blk_id option) list, 't) m;
}

type ('a,'blk_id,'blk) adv_hd = {
  old_hd   :'blk_id;
  old_elts :'a list;
  new_hd   :'blk_id
}

type 'a or_error = ('a,unit) result

(** plist operations which require the plist state from the monad *)
type ('a,'buf (* FIXME *),'blk_id,'blk,'t) plist_ops = {
  add       : nxt:'blk_id -> elt:'a -> ('blk_id option,'t) m;
  sync      : unit -> (unit,'t)m;
  blk_len   : unit -> (int,'t)m;
  adv_hd    : unit -> ( ('a,'blk_id,'blk) adv_hd or_error,'t)m; (** advance hd *)
  adv_tl    : 'blk_id -> (unit,'t)m;
  get_hd    : unit -> ('blk_id,'t)m;
  get_tl    : unit -> ('blk_id,'t)m;
  (* get_hd_tl : unit -> ('blk_id * 'blk_id,'t)m; *)
  read_hd   : unit -> ('a list * 'blk_id option,'t)m;
  append    : ('a,'blk_id,'buf) plist -> (unit,'t)m;
  (* read_tl   : unit -> (('a list * 'blk_id option)or_error,'t)m; *)
  (* read_blk  : 'blk_id -> (('a list * 'blk_id option)or_error,'t)m *)
}
(** add : The blk_id is returned if it is not used; another variant
   assumes an alloc function. We don't assume the nxt_blk is clean -
   we may write a single elt into the new blk then sync that before
   updating the nxt pointer.

sync : we automatically sync before moving to a new tl; this is for
   partial blocks which we need to sync

adv_hd: return an error if no nxt pointer (iff blk_len = 1 iff hd=tl)

adv_tl: write tl and advance tl to a fresh blk

read_hd: assumes hd has been marshalled to disk; this likely won't be
   the case if hd=tl

append: take a second plist, and adjust the "current" plist so that
   the hd is unchanged, but the tl points to the tl of the second
   plist (and the buf and offset etc are copied); an assumption is
   that the second plist is never used from this point onwards. FIXME
   clearly we need to remove the pl2 from the global state, or else we
   have a space leak

*)


(* read_blk: we expect to be called on a blk_id that can be unmarshalled (ie one that is part of some plist); note that it may be possible that this succeeds on random blk data; use with care! *)



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


(*
(** The result of making the plist *)
module Ret_ = struct
  type ('a,'blk_id,'blk,'buf,'t) ret_ = {
    m_u_ops      : ('a, 'blk_id, 'blk) plist_marshal_ops;
    extra_ops    : ('a,'buf,'blk_id,'t) plist_extra_ops;
    plist_ops    : (('a, 'blk_id, 'buf) plist, 't) with_state ->
      ('a,'buf,'blk_id,'blk,'t) plist_ops
  }
  (** What we return from making the plist; note that the [plist_ops]
      require a [with_state] *)
end
include Ret_
*)
