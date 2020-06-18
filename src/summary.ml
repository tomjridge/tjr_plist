(** Summary of main types *)

(**

{[
type ('blk_id,'buf) plist = {
  hd      : 'blk_id;
  tl      : 'blk_id;
  buffer  : 'buf;
  off     : int;
  blk_len : int;
  dirty   : bool; (** may not have been written to disk *)
}

type ('a,'blk_id,'blk,'buf,'t) plist_factory = <
  monad_ops          :'t monad_ops;
  buf_ops            :'buf buf_ops;
  blk_ops            : 'blk blk_ops;
  plist_marshal_info : ('a,'blk_id,'blk,'buf) plist_marshal_info;
  plist_marshal_ops  : ('a,'blk_id,'blk) plist_marshal_ops; 
  with_blk_dev_ops   :  
    blk_dev_ops : ('blk_id,'blk,'t)blk_dev_ops ->
    sync        : (unit -> (unit,'t)m)
    -> <
      init : <
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
        ('blk_id,'t) Pl_origin.ops -> 
        ('a,'buf,'blk_id,'t)plist_ops -> 
        ('a,'buf,'blk_id,'t)plist_ops
    (** Modify plist_ops to sync the origin block when hd/tl change *)

    >
>

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

]}

*)

