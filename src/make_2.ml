(** A persistent list for arbitrary elts, assuming existence of a
   freelist (so we don't have to closely track blk_ids) *)

open Plist_intf
open Pl_origin

(* $(CONVENTION("""for restricted types, append some indicator of what
   the fields are; here, af ~ alloc+free; this is less confusing than
   having a type with the same name in a different module """)) *)
type ('blk_id,'t) freelist_ops_af = {
  blk_alloc : (unit -> ('blk_id,'t)m);
  blk_free  : ('blk_id -> (unit,'t)m);
}


(* $(PIPE2SH("""sed -n '/type[ ].*simple_plist_ops = /,/^}/p' >GEN.simple_plist_ops.ml_""")) *)
type ('a,'blk_id,'t) simple_plist_ops = {
  add           : 'a -> (bool,'t)m;
  (** Return value indicates whether we moved to a new block *)

  sync_tl       : unit -> (unit,'t)m;
  blk_len       : unit -> (int,'t)m;
  get_origin    : unit -> ('blk_id pl_origin,'t)m;
}

(* $(PIPE2SH("""sed -n '/type[ ].*simple_plist_factory = /,/^>/p' >GEN.simple_plist_factory.ml_""")) *)
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

(** {2 Plist_ops_2 from plist_ops} *)

let convert_to_simple_plist 
    ~monad_ops ~freelist_ops ~plist_ops =
  let open (struct

    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return

    let { blk_alloc; blk_free } = freelist_ops

    let { add; add_if_room;sync_tl; blk_len; get_origin; _ } = plist_ops

    let add e = 
      add_if_room e >>= function
      | true -> return false
      | false -> 
        blk_alloc () >>= fun blk_id -> 
        add ~nxt:blk_id ~elt:e >>= function
        | None -> return true
        | Some blk_id -> 
          (* can this happen? maybe? depends on concurrency *)       
          blk_free blk_id >>= fun () -> 
          return true

    let plist_ops_2 = { add; sync_tl; blk_len; get_origin }
  end)
  in
  plist_ops_2

let _ = convert_to_simple_plist

let create 
    ~monad_ops
    ~(plist_factory:_ plist_factory) 
    ~(blk_dev_ops:_ blk_dev_ops) 
    ~barrier
    ~(freelist_ops:_ freelist_ops_af) 
    ~blk_id
  = 
  let ( >>= ) = monad_ops.bind in
   let return = monad_ops.return in
  
  plist_factory#with_blk_dev_ops ~blk_dev_ops ~barrier |> fun o1 -> 
  o1#create blk_id >>= fun o2 -> 
  let plist_ops = o2#plist_ops in
  let simple_plist_ops = convert_to_simple_plist ~monad_ops ~freelist_ops ~plist_ops in
  return @@ object
    method plist_ref=o2#plist_ref
    method with_plist=o2#with_plist
    method plist_ops=o2#plist_ops
    method simple_plist_ops=simple_plist_ops
  end

let restore
    ~monad_ops
    ~(plist_factory:_ plist_factory) 
    ~(blk_dev_ops:_ blk_dev_ops) 
    ~barrier
    ~(freelist_ops:_ freelist_ops_af) 
    ~pl_origin
  = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  
  plist_factory#with_blk_dev_ops ~blk_dev_ops ~barrier |> fun o1 -> 
  o1#restore pl_origin >>= fun o2 -> 
  let plist_ops = o2#plist_ops in
  let simple_plist_ops = convert_to_simple_plist ~monad_ops ~freelist_ops ~plist_ops in
  return @@ object
    method plist_ref=o2#plist_ref
    method with_plist=o2#with_plist
    method plist_ops=o2#plist_ops
    method simple_plist_ops=simple_plist_ops
  end

  

let examples =
  let monad_ops = lwt_monad_ops in
  let convert_to_simple_plist ~freelist_ops ~plist_ops = 
    convert_to_simple_plist ~monad_ops ~freelist_ops ~plist_ops
  in
  let with_ ~blk_dev_ops ~barrier ~freelist_ops ~plist_factory = 
    let create blk_id = 
      create ~monad_ops ~plist_factory ~blk_dev_ops ~barrier ~freelist_ops ~blk_id in
    let restore pl_origin = 
      restore ~monad_ops ~plist_factory ~blk_dev_ops ~barrier ~freelist_ops ~pl_origin in
    object
      method create=create
      method restore=restore
    end
  in
  let for_int : (int,_,_,_,_) simple_plist_factory = 
    let plist_factory = Make_1.pl_examples#for_int in
    object
      method plist_factory=plist_factory
      method convert_to_simple_plist=convert_to_simple_plist
      method with_ = with_ ~plist_factory
    end
  in
  let for_int_int_kvop : ((int,int)kvop,_,_,_,_) simple_plist_factory = 
    let plist_factory = Make_1.pl_examples#for_int_int_kvop in
    object
      method plist_factory=plist_factory
      method convert_to_simple_plist=convert_to_simple_plist
      method with_ = with_ ~plist_factory
    end
  in
  object
    method for_int=for_int
    method for_int_int_kvop=for_int_int_kvop
  end

let _ = examples
