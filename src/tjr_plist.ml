(** A persistent (on-disk) list. 

Best to locally open just this module (which includes intf).
*)

module Plist_intf = Plist_intf
include Plist_intf

let make : plist_marshal_info:('a, 'blk_id, 'blk, 'buf) plist_marshal_info ->
buf_ops:'buf buf_ops ->
blk_ops:'blk blk_ops ->
('a, 'blk_id, 'blk) plist_marshal_ops *
[ `K1 of
     monad_ops:'b monad_ops ->
     blk_dev_ops:('blk_id, 'blk, 'b) blk_dev_ops ->
     ('a, 'buf, 'blk_id, 'b) plist_extra_ops *
     [ `K2 of
          with_state:(('c, 'blk_id, 'buf) plist, 'b) with_state ->
          ('a, 'd, 'blk_id, 'e, 'b) plist_ops ] ] = Make_.make


(** This adds sync operations whenever hd or tl change; not thread
   safe - assumes only one thread is accessing the list (but probably
   works even in the multithreaded case, except that the root syncs
   may be out of order) *)
let plist_add_sync ~monad_ops ~sync_root_blk ~plist_ops = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let { add; adv_hd; adv_tl; append; get_hd_tl; _ } = plist_ops in
  let add ~nxt ~elt =
    add ~nxt ~elt >>= fun b -> 
    (match b with 
     | None -> get_hd_tl () >>= sync_root_blk
     | Some blk_id -> return ()) >>= fun () ->
    return b
  in
  let adv_hd () = 
    adv_hd () >>= fun x ->
    get_hd_tl () >>= sync_root_blk >>= fun () ->
    return x
  in
  let append pl = 
    append pl >>= fun () ->
    get_hd_tl () >>= sync_root_blk
  in
  { plist_ops with add; adv_hd; append }



