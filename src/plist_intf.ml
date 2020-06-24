(** Plist main types 

The main plist types come in three groups:

- [plist_marshal_ops], which becomes available with just
  [plist_marshal_info] (and blk and buf ops)
- [plist_extra_ops], for operating on an on-disk plist; available with
  [blk_dev_ops]
- [plist_ops], available once we decide how to store the plist state
  (via the [with_state] parameter)

*)

(* $(PIPE2SH("""sed -n '/type[ ].*plist = /,/^}/p' >GEN.plist.ml_""")) *)
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



(** This type is the standard information we need to persist in order
    to quickly restore the plist; we can also just follow the list from
    the hd block of course, but this is O(n). *)
module Pl_origin = struct 
  open Bin_prot.Std
  (* \$(PIPE2SH("""sed -n '/type[ ].*pl_origin/,/^[ ]*}/p' >GEN.pl_origin.ml_""")) *)
  type 'blk_id pl_origin = {
    hd  : 'blk_id;
    tl  : 'blk_id;
    blk_len : int
  }[@@deriving bin_io]


  let mshlr (type r) ~(r_mshlr: r bp_mshlr) : r pl_origin bp_mshlr = 
    let module A = (val r_mshlr) in
    let module B = struct
      type t = A.t pl_origin[@@deriving bin_io]
      let max_sz = (2*A.max_sz)+9
    end
    in
    (module B)
  let _ = mshlr


  type 'blk_id t = 'blk_id pl_origin[@@deriving bin_io]

  (* $(CONVENTION("""

     The block on disk from which an object can be reconstructed is
     called the origin block.  origin is held in mem, and synced to
     disk explicitly; typically the origin fields will be part of some
     larger in-mem structure, and sync will extract the fields and
     write to some blk on disk

     The typical route to construct a persistent object is then to
     read the initial state from disk, implement with_state, implement
     origin ops, then construct the operations using with_state and
     origin_ops. In case the origin is part of some larger structure,
     we implement the get/set/sync interface (rather than just sync)
     """)) *)
  type ('blk_id,'t) ops = {
    read  : unit -> 'blk_id t; 
    write : 'blk_id t -> (unit,'t)m; 
    set_and_sync: 'blk_id t -> (unit,'t)m; 
    (** convenience combination of set and sync *)
  }
end
open Pl_origin




(** Internal operations, for debugging FIXME this is just ('a list *
   blk_id,'blk) mshlr *)
type ('a,'blk_id,'blk) plist_marshal_ops = {  
  unmarshal : 'blk->'a list * 'blk_id option; 
  marshal   : 'a list * 'blk_id option->'blk; 
}

type ('a,'blk_id) adv_hd = {
  old_hd   :'blk_id;
  old_elts :'a list;
  new_hd   :'blk_id
}
(** Information from the "advance head" operation *)

type 'a or_error = ('a,unit) result

(** plist operations which require the plist state from the monad *)
(* $(PIPE2SH("""sed -n '/type[ ].*plist_ops/,/^}/p' >GEN.plist_ops.ml_""")) *)
type ('a,'buf (* FIXME *),'blk_id, 't) plist_ops = {
  add           : nxt:'blk_id -> elt:'a -> ('blk_id option,'t) m;
  add_if_room   : 'a -> (bool,'t)m;

  sync_tl       : unit -> (unit,'t)m;

  adv_hd        : unit -> ( ('a,'blk_id) adv_hd or_error,'t)m; 
  adv_tl        : 'blk_id -> (unit,'t)m;

  blk_len       : unit -> (int,'t)m;
  get_origin    : unit -> ('blk_id pl_origin,'t)m;

  read_hd       : unit -> ('a list * 'blk_id option,'t)m;
  append        : ('blk_id,'buf) plist -> (unit,'t)m;
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


(*
(* \$(CONVENTION("A trailing s indicates that the basic operations are accompanied with a sync operation")) *)

type ('blk_id,'blk,'t)blk_dev_ops_s = <
    get : ('blk_id,'blk,'t)blk_dev_ops;
    sync: unit -> (unit,'t)m
> 
*)

(* assume buf_ops and blk_ops are given, and plist_marshal_info *)
(* $(PIPE2SH("""sed -n '/type[ ].*plist_factory/,/^>/p' >GEN.plist_factory.ml_""")) *)
type ('a,'blk_id,'blk,'buf,'t) plist_factory = <
  monad_ops          :'t monad_ops;
  buf_ops            :'buf buf_ops;
  blk_ops            : 'blk blk_ops;
  plist_marshal_info : ('a,'blk_id,'blk,'buf) plist_marshal_info;
  plist_marshal_ops  : ('a,'blk_id,'blk) plist_marshal_ops; 
  with_blk_dev_ops   :  
    blk_dev_ops : ('blk_id,'blk,'t)blk_dev_ops ->
    barrier     : (unit -> (unit,'t)m)
    -> <
      init : <
        (* FIXME rename to "initialize_empty_blk" or similar *)
        mk_empty     : 'blk_id -> (('blk_id,'buf)plist,'t)m;

        read_from_hd : 'blk_id -> ( ('a list * 'blk_id option) list, 't) m; 

        from_endpts  : 'blk_id pl_origin -> (('blk_id,'buf)plist,'t)m;
      >;

      with_state : 
        (('blk_id,'buf)plist,'t)with_state -> 
        ('a,'buf,'blk_id,'t)plist_ops;

      (* convenience; use with with_state *)
      with_ref : 
        ('blk_id,'buf)plist -> (
          <
            plist_ref  : ('blk_id,'buf)plist ref;              
            with_plist : (('blk_id,'buf)plist,'t)with_state;
          >);

      add_origin : 
        <set_and_sync: 'blk_id pl_origin -> (unit,'t)m> -> 
        ('a,'buf,'blk_id,'t)plist_ops -> 
        ('a,'buf,'blk_id,'t)plist_ops
    (** Modify plist_ops to sync the origin block when hd/tl change *)

    >
>


(* $(PIPE2SH("""sed -n '/type[ ].*origin_factory/,/^>/p' >GEN.origin_factory.ml_""")) *)
type ('blk_id,'blk,'t) origin_factory = <
  monad_ops :'t monad_ops;
  with_: 
    blk_dev_ops: ('blk_id,'blk,'t)blk_dev_ops -> 
    blk_id : 'blk_id -> 
    sync_blk_id : (unit -> (unit,'t)m) ->
    < set_and_sync: 'blk_id pl_origin -> (unit,'t)m >
>
