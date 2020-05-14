(** Plist main types 

The main plist types come in three groups:

- [plist_marshal_ops], which becomes available with just
  [plist_marshal_info] (and blk and buf ops)
- [plist_extra_ops], for operating on an on-disk plist; available with
  [blk_dev_ops]
- [plist_ops], available once we decide how to store the plist state
  (via the [with_state] parameter)

*)

type ('blk_id,'buf) plist = {
  hd      : 'blk_id;
  tl      : 'blk_id;
  buffer  : 'buf;
  off     : int;
  blk_len : int;
  dirty   : bool; (** may not have been written to disk *)
}
(** This is an internal implementation type.  NOTE modifications take
   place in the tl block

FIXME? including blk_len makes things a bit trickier since we have to
   store this in the root blk *)


(** Internal operations, for debugging FIXME this is just ('a list *
   blk_id,'blk) mshlr *)
type ('a,'blk_id,'blk) plist_marshal_ops = {  
  unmarshal : 'blk->'a list * 'blk_id option; 
  marshal   : 'a list * 'blk_id option->'blk; 
}

type ('a,'buf,'blk_id,'t) plist_extra_ops = {  
  create_plist   : 'blk_id -> (('blk_id,'buf)plist,'t)m;
  (* read_plist_blk : 'blk_id -> ('a list * 'blk_id option,'t) m; *)

  read_plist     : 'blk_id -> ( ('a list * 'blk_id option) list, 't) m;  
  (* FIXME rename to pl_read_elts *)

  read_plist_tl  : hd:'blk_id -> tl:'blk_id -> blk_len:int -> (('blk_id,'buf)plist,'t)m;
}
(** Operations which don't require the plist state; typically
   initialization and debugging 

- create_plist: in the current impl, this writes the empty list to
  disk

- read_plist_tl: to constructs a plist from one previously written to
  disk; hd is assumed to point to a valid hd; tl is read and the plist
  constructed
*)


type ('a,'blk_id) adv_hd = {
  old_hd   :'blk_id;
  old_elts :'a list;
  new_hd   :'blk_id
}
(** Information from the "advance head" operation *)

type 'a or_error = ('a,unit) result

(** plist operations which require the plist state from the monad *)
(* $(PIPE2SH("""sed -n '/type[ ].*plist_ops/,/^}/p' >GEN.plist_ops.ml_""")) *)
type ('a,'buf (* FIXME *),'blk_id,(* 'blk,*) 't) plist_ops = {
  add       : nxt:'blk_id -> elt:'a -> ('blk_id option,'t) m;
  add_if_room: 'a -> (bool,'t)m;
  sync_tl   : unit -> (unit,'t)m;
  blk_len   : unit -> (int,'t)m;
  adv_hd    : unit -> ( ('a,'blk_id) adv_hd or_error,'t)m; 
  (** advance hd *)

  adv_tl    : 'blk_id -> (unit,'t)m;
  get_hd    : unit -> ('blk_id,'t)m;
  get_tl    : unit -> ('blk_id,'t)m;
  get_hd_tl : unit -> ('blk_id * 'blk_id,'t)m;
  read_hd   : unit -> ('a list * 'blk_id option,'t)m;
  append    : ('blk_id,'buf) plist -> (unit,'t)m;
  (* read_tl   : unit -> (('a list * 'blk_id option)or_error,'t)m; *)
  (* read_blk  : 'blk_id -> (('a list * 'blk_id option)or_error,'t)m *)
}
(** 

- add : The blk_id is returned if it is not used; another variant
  assumes an alloc function. We don't assume the nxt_blk is clean - we
  may write a single elt into the new blk then sync that before
  updating the nxt pointer.

- add_if_room: returns a boolean to indicate if the element was added
  in the current tl

- sync : we automatically sync before moving to a new tl; this is for
  partial blocks which we need to sync

- adv_hd: return an error if no nxt pointer (iff blk_len = 1 iff
  hd=tl)

- adv_tl: write tl and advance tl to a fresh blk

- read_hd: assumes hd has been marshalled to disk; this likely won't
  be the case if hd=tl

- append: take a second plist, and adjust the "current" plist so that
  the hd is unchanged, but the tl points to the tl of the second plist
  (and the buf and offset etc are copied); an assumption is that the
  second plist is never used from this point onwards. FIXME clearly we
  need to remove the pl2 from the global state, or else we have a
  space leak

*)



module Plist_marshal_info = struct
  type ('a,'blk_id,'blk,'buf) plist_marshal_info = {
    elt_mshlr     : ('a option,'buf)mshlr;
    blk_id_mshlr  : ('blk_id option,'buf)mshlr;
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



(* assume buf_ops and blk_ops are given, and plist_marshal_info, and blk_dev_ops *)
(* $(PIPE2SH("""sed -n '/type[ ].*plist_factory/,/^>/p' >GEN.plist_factory.ml_""")) *)
type ('a,'blk_id,'blk,'buf,'t) plist_factory = <
  buf_ops            :'buf buf_ops;
  blk_ops            : 'blk blk_ops;
  plist_marshal_info : ('a,'blk_id,'blk,'buf) plist_marshal_info;
  plist_marshal_ops  : ('a,'blk_id,'blk) plist_marshal_ops; 
  with_blk_dev_ops   :  
    monad_ops   :'t monad_ops -> 
    blk_dev_ops : ('blk_id,'blk,'t)blk_dev_ops 
    -> <
      monad_ops       : 't monad_ops;
      blk_dev_ops     : ('blk_id,'blk,'t)blk_dev_ops;
      plist_extra_ops : ('a,'buf,'blk_id,'t) plist_extra_ops;
      with_state      : (('blk_id,'buf)plist,'t)with_state -> 
        ('a,'buf,'blk_id,'t)plist_ops;
      from_disk       : <hd:'blk_id;tl:'blk_id;blk_len:int> -> 
        (<
          plist_ops:('a,'buf,'blk_id,'t)plist_ops;
          with_plist: (('blk_id,'buf)plist,'t)with_state;
          plist_ref: ('blk_id,'buf)plist ref;              
        >,'t)m
    >
>
