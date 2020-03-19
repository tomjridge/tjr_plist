(** Utility module, for adding sync when hd,tl ptrs change *)

open Plist_intf

module Plist_sync_root_blk = struct
  type ('blk_id,'t) t = {
    sync_root_blk: ('blk_id*'blk_id) -> (unit,'t)m;
  }
end

type ('blk_id,'t) srb = ('blk_id,'t) Plist_sync_root_blk.t




(** Utility: this adds sync operations whenever hd or tl change; not
   thread safe - assumes only one thread is accessing the list (but
   probably works even in the multithreaded case, except that the root
   syncs may be out of order) *)
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
