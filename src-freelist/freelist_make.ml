(* FIXME perhaps this would be better as a queue of blk_ids, and a
   self-throttling thread on one end *)

open Freelist_intf

type ('elt,'blk_id,'t) version = 
  | For_blkids of { e2b:'elt -> 'blk_id; b2e: 'blk_id -> 'elt } 
  (** The version to implement the blk freelist; the elts are actually blk_ids *)

  | For_arbitrary_elts of { alloc: unit -> ('blk_id,'t)m; free: 'blk_id -> (unit,'t)m }
  (** The version where the freelist provides the alloc and free blk functionality *)

module type S = sig

  type blk_id
  type blk
  type buf

  type elt (** elements stored in the free list *)

  type t

end

[@@@warning "-26-8"] (* FIXME *)

module Make(S:S) = struct

  open S

  type nonrec version = (elt,blk_id,t)version

  (* try to keep the transient list size between these two *)
  let tr_upper = 4
  let tr_lower = 2

  (* NOTE we need a version which deals with blk_ids, and a version which deals with arbitrary elts *)

  let make 
        ~monad_ops
        ~event_ops
        ~(async:(unit,t)m -> (unit,t)m)
        ~(plist:(elt,buf,blk_id,blk,t)plist_ops)
        ~(with_freelist:(elt freelist,t)with_state)
        ~(root_block:(blk_id,t)root_block_ops)
        ~(version:version)
    = 
    (* let For_arbitrary_elts blk_alloc = version in *)
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let Event.{ ev_create; ev_wait; ev_signal } = event_ops in
    let {add; _ } = plist in

    (* redefine add to use alloc if available *)
    let version = match version with
      | For_blkids {e2b;b2e} -> `For_blkids(e2b,b2e)
      | For_arbitrary_elts {alloc;free} -> 
        let add elt = 
          alloc () >>= fun blk_id ->
          add ~nxt:blk_id ~elt >>= function 
          | None -> return ()
          | Some blk_id -> free blk_id
        in
        `For_arbitrary_elts (alloc,free,add)
    in

    (* The disk_thread asynchronously attempts to add elements to the
       transient list by reading from the head of the on-disk list and
       returning new elts *)
    (* disk_thread gets triggered when we fall below tr_lower
       bound. We need to record that a disk_thread is active *)
    let disk_thread () = 
      (* we need to read blk_ids from hd, advance hd, add to
         transients, then sync freelist in root block before allowing
         other threads to run *)
      return () (* FIXME *) 
    in

    let alloc () =
      with_freelist.with_state (fun ~state:s ~set_state -> 
        match List.length s.transient < tr_lower && not s.disk_thread_active with
        | false -> return ()
        | true -> 
          async(disk_thread ()) >>= fun () ->
          set_state {s with disk_thread_active=true}) >>= fun () ->

      with_freelist.with_state (fun ~state:s ~set_state -> 
        match s.transient with
        | [] -> (
            (* FIXME are we guaranteed that the disk thread is active? *)
            assert(s.disk_thread_active=true);
            ev_create () >>= fun (ev: elt Event.event) ->              
            set_state {s with waiting=ev::s.waiting } >>= fun () -> 
            (* FIXME do we have to release the state before waiting? *)
            ev_wait ev >>= fun elt ->
            return elt)
        | elt::transient -> 
          set_state {s with transient} >>= fun () ->
          return elt)
    in
    let alloc_many n = failwith "FIXME" in
    
    (* NOTE the following code is rather tricky; if we have an alloc
       provided, we use this to extend the list; otherwise we are
       working with elts as blk_ids, so we can use one of the elts as
       a backing block for the list itself when we need to extend the
       list *)
    let free_elts xs =
      match version with
      | `For_arbitrary_elts (_,_,add) -> (
          xs |> iter_k (fun ~k xs ->
              match xs with 
              | [] -> return ()
              | x::xs -> 
                add x >>= fun () ->
                k xs))
      | `For_blkids (e2b,b2e) -> (
          xs |> iter_k (fun ~k xs -> 
              match xs with
              | [] -> return ()
              | [e] -> (
                  plist.add_if_room e >>= function
                  | true -> return ()
                  | false -> (
                      (* so use this e as the nxt blk *)
                      plist.adv_tl (e2b e)))
              | x1::x2::xs -> (
                  plist.add ~nxt:(e2b x2) ~elt:x1 >>= function
                  | None -> k xs
                  | Some x2 -> k ((b2e x2)::xs)) ))
    in

    let free blk_id = 
      with_freelist.with_state (fun ~state:s ~set_state -> 
        let transient = blk_id::s.transient in
        match List.length transient >= tr_upper with
        | true -> (
            (* flush some to disk *)
            transient |> List_.split_at ((tr_upper+tr_lower) / 2) |> fun (xs,ys) ->
            free_elts xs >>= fun () ->
            set_state {s with transient=ys })                
        | false -> (
            set_state {s with transient}))
    in
    
    let free_many pl = failwith "FIXME" in

    let sync = function
      | `Tl_only -> plist.sync ()
      | `Tl_and_root_block -> 
        plist.sync () >>= fun () ->
        plist.get_hd () >>= fun hd ->
        plist.get_tl () >>= fun tl ->
        root_block.write_freelist_roots ~hd ~tl >>= fun () ->
        root_block.sync ()
    in

    { alloc; alloc_many; free; free_many; sync }

end
