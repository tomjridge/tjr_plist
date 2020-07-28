(** A persistent list for arbitrary elts, assuming existence of a
   freelist (so we don't have to closely track blk_ids) *)

open Plist_intf
open Pl_origin

(* $(PIPE2SH("""sed -n '/type[ ].*simple_plist_ops = /,/^}/p' >GEN.simple_plist_ops.ml_""")) *)
type ('a,'blk_id,'t) simple_plist_ops = {
  add           : 'a -> (unit,'t)m;
  sync_tl       : unit -> (unit,'t)m;
  blk_len       : unit -> (int,'t)m;
  get_origin    : unit -> ('blk_id pl_origin,'t)m;
}

(* $(PIPE2SH("""sed -n '/type[ ].*simple_plist_factory = /,/^>/p' >GEN.simple_plist_factory.ml_""")) *)
type ('a,'blk_id,'blk,'buf,'t) simple_plist_factory = <  
  plist_factory: 
    ('a,'blk_id,'blk,'buf,'t) plist_factory;

  convert_to_simple_plist: 
    monad_ops : 't monad_ops ->
    freelist_ops : ('blk_id,'t) Shared_freelist.freelist_ops -> 
    plist_ops : ('a,'buf,'blk_id,'t) plist_ops -> 
    ('a,'blk_id,'t) simple_plist_ops
>


(** {2 Plist_ops_2 from plist_ops} *)

let convert_to_simple_plist 
    ~monad_ops ~freelist_ops ~plist_ops =
  let open (struct

    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return

    let Shared_freelist.{ alloc; free; _ } = freelist_ops

    let { add; add_if_room;sync_tl; blk_len; get_origin; _ } = plist_ops

    let add e = 
      add_if_room e >>= function
      | true -> return ()
      | false -> 
        alloc () >>= fun blk_id -> 
        add ~nxt:blk_id ~elt:e >>= function
        | None -> return ()
        | Some blk_id -> 
          (* can this happen? maybe? depends on concurrency *)       
          free blk_id 

    let plist_ops_2 = { add; sync_tl; blk_len; get_origin }
  end)
  in
  plist_ops_2

let examples =
  let for_int : (int,_,_,_,_) simple_plist_factory = 
    let plist_factory = Make_1.pl_examples#for_int in
    object
      method plist_factory=plist_factory
      method convert_to_simple_plist=convert_to_simple_plist
    end
  in
  object
    method for_int=for_int
  end
