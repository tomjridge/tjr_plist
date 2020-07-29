(** Summary of main types *)

(**

{[
type ('blk_id,'blk,'t) origin_factory = <
  monad_ops :'t monad_ops;
  with_: 
    blk_dev_ops: ('blk_id,'blk,'t)blk_dev_ops -> 
    blk_id : 'blk_id -> 
    sync_blk_id : (unit -> (unit,'t)m) ->
    < set_and_sync: 'blk_id pl_origin -> (unit,'t)m >
>

type ('blk_id,'buf) plist = {
  hd      : 'blk_id;
  tl      : 'blk_id;
  buffer  : 'buf;
  off     : int;
  blk_len : int;
  tl_dirty: bool; (** tl may not have been written to disk *)
}

type ('a,'blk_id,'blk,'buf,'t) plist_factory = <
  monad_ops          :'t monad_ops;
  buf_ops            :'buf buf_ops;
  blk_ops            : ('blk,'buf) blk_ops;
  plist_marshal_info : ('a,'blk_id,'blk,'buf) plist_marshal_info;
  plist_marshal_ops  : ('a,'blk_id,'blk) plist_marshal_ops; 
  with_blk_dev_ops   :  
    blk_dev_ops : ('blk_id,'blk,'t)blk_dev_ops ->
    barrier     : (unit -> (unit,'t)m)
    -> <
      (* NOTE plist is very explicit about blk_ids (because it is used
         as the basis for the freelist); so we don't assume a freelist
         here, instead we require fresh blk_ids to be passed
         explicitly *)
      init : <
        (* FIXME use create method instead *)
        (* mk_empty_     : 'blk_id -> (('blk_id,'buf)plist,'t)m; *)

        (* NOTE this does not create an origin blk *)
        create       : 'blk_id -> (('blk_id,'buf)plist,'t)m;

        read_from_hd : 'blk_id -> ( ('a list * 'blk_id option) list, 't) m; 

        (* NOTE if the resulting plist has tl <> the origin tl, then
           the origin should probably be updated for efficiency *)
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
        ('a,'buf,'blk_id,'t)plist_ops;
      (** Modify plist_ops to sync the origin block when hd/tl change *)


      (* Convenience *)

      create : 'blk_id -> (<
          plist_ref  : ('blk_id,'buf)plist ref;              
          with_plist : (('blk_id,'buf)plist,'t)with_state;
          plist_ops  : ('a,'buf,'blk_id,'t)plist_ops
        >,'t)m;
      (** NOTE no add_origin *)

      restore : 'blk_id Pl_origin.t -> (<
          plist_ref  : ('blk_id,'buf)plist ref;              
          with_plist : (('blk_id,'buf)plist,'t)with_state;
          plist_ops  : ('a,'buf,'blk_id,'t)plist_ops
        >,'t)m;
      (** NOTE no add_origin *)

    >
>

  type ('a,'blk_id,'blk,'buf) plist_marshal_info = {
    elt_mshlr     : ('a option,'buf)mshlr;
    blk_id_mshlr  : ('blk_id option,'buf)mshlr;
  }

type ('a,'buf (* FIXME *),'blk_id, 't) plist_ops = {
  (* NOTE operations which REQUIRE an update to origin are marked with an exclam *)
  add           : nxt:'blk_id -> elt:'a -> ('blk_id option,'t) m;
  add_if_room   : 'a -> (bool,'t)m;

  sync_tl       : unit -> (unit,'t)m;

  adv_hd        : unit -> ( ('a,'blk_id) adv_hd or_error,'t)m; (* ! *)
  adv_tl        : 'blk_id -> (unit,'t)m;

  blk_len       : unit -> (int,'t)m;
  get_origin    : unit -> ('blk_id pl_origin,'t)m;

  read_hd       : unit -> ('a list * 'blk_id option,'t)m;
  append        : ('blk_id,'buf) plist -> (unit,'t)m;
}

type ('a,'blk_id,'blk,'buf,'t) simple_plist_factory = <  
  plist_factory: 
    ('a,'blk_id,'blk,'buf,'t) plist_factory;

  convert_to_simple_plist: 
    freelist_ops : ('blk_id,'t) freelist_ops_af -> 
    plist_ops    : ('a,'buf,'blk_id,'t) plist_ops -> 
    ('a,'blk_id,'t) simple_plist_ops;

  (* Convenience *)

  with_ : 
    blk_dev_ops : ('blk_id,'blk,'t)blk_dev_ops ->
    barrier : (unit -> (unit,'t)m) -> 
    freelist_ops: ('blk_id,'t)freelist_ops_af -> 
    <

      create: 'blk_id -> (<
          plist_ref        : ('blk_id,'buf)plist ref;              
          with_plist       : (('blk_id,'buf)plist,'t)with_state;
          plist_ops        : ('a,'buf,'blk_id,'t)plist_ops;
          simple_plist_ops : ('a,'blk_id,'t)simple_plist_ops;      
        >,'t)m;

      restore: 'blk_id Pl_origin.t -> (<
          plist_ref        : ('blk_id,'buf)plist ref;              
          with_plist       : (('blk_id,'buf)plist,'t)with_state;
          plist_ops        : ('a,'buf,'blk_id,'t)plist_ops;
          simple_plist_ops : ('a,'blk_id,'t)simple_plist_ops;      
        >,'t)m;
    >
>

type ('a,'blk_id,'t) simple_plist_ops = {
  add           : 'a -> (bool,'t)m;
  (** Return value indicates whether we moved to a new block *)

  sync_tl       : unit -> (unit,'t)m;
  blk_len       : unit -> (int,'t)m;
  get_origin    : unit -> ('blk_id pl_origin,'t)m;
}

]}

*)

