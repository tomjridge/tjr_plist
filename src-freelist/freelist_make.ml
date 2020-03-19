(* FIXME perhaps this would be better as a queue of blk_ids, and a
   self-throttling thread on one end *)

open Plist_intf
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

(* [@@@warning "-26-8"] (\* FIXME *\) *)

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
        ~(plist:(elt,buf,blk_id,t)plist_ops)
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

    let _ = version in

    (* The disk_thread asynchronously attempts to add elements to the
       transient list by reading from the head of the on-disk list and
       returning new elts *)
    (* disk_thread gets triggered when we fall below tr_lower bound;
       we need to record that a disk_thread is active; the disk_thread
       then repeatedly goes to disk and uses the resulting frees to
       satisfy the waiting threads; the disk_thread only finishes
       doing this if the number of transients AFTER satisfying the
       waiting threads is at least tr_lower; FIXME we may need to
       throttle if the number of waiters keeps growing, or at least
       never shrinks to 0; perhaps the disk thread exponentially
       increases the number of blocks it scans each time

       NOTE that when we advance the hd, the old head becomes free
    *)
    let disk_thread () = 
      (* we need to: read elts from hd, advance hd, sync hd, signal waiters *)
      let step () = 
        plist.adv_hd () >>= function
        | Error () -> return `End_of_plist 
        | Ok {old_hd; old_elts; _ } -> (
            plist.sync_tl () >>= fun () ->

            (* maybe free old_hd (or, if we are dealing with the
               freelist itself, add to elts) *)
            (match version with
             | `For_blkids (e2b,b2e) -> return (Some(b2e old_hd))
             | `For_arbitrary_elts (alloc,free,add) -> 
               free old_hd >>= fun () -> return None) >>= fun e_opt ->

            let elts = match e_opt with None -> old_elts | Some e -> e::old_elts in

            with_freelist.with_state (fun ~state:s ~set_state ->
                assert(s.disk_thread_active);
                (elts,s.waiting) |> iter_k (fun ~k (elts,waiting) -> 
                    match elts,waiting with
                    | [],_ -> return {s with waiting}
                    | _,[] -> return {s with waiting=[]; transient=elts@s.transient}
                    | e::elts,w::waiting -> (
                        ev_signal w e >>= fun () -> 
                        k (elts,waiting)))
                >>= fun s -> 
                let continue_ = s.waiting!=[] || List.length s.transient < tr_lower in
                (* NOTE if we know we are not going to continue, we
                   take this opportunity to unset the
                   disk_thread_active flag; we have to do this while
                   we have the state *)
                set_state { s with disk_thread_active=(not continue_) } >>= fun () ->
                return (`Ok continue_)) )
      in     

      let rec loop () = step () >>= function 
        | `Ok true -> loop ()
        | `Ok false -> return `Finished
        | `End_of_plist -> 
          (* at this point, we have exhausted the frees stored on disk *)
          return `Unfinished
      in

      loop ()
    in

    let alloc () =
      (* check if we need to go to disk *)
      with_freelist.with_state (fun ~state:s ~set_state -> 
          match List.length s.transient < tr_lower && not s.disk_thread_active with
          | false -> return false
          | true -> 
            (* FIXME we must ensure that the freelist state is protected eg with a mutex *)
            set_state {s with disk_thread_active=true} >>= fun () -> return true) 

      (* maybe launch a disk thread *)
      >>= (function
          | false -> return ()
          | true -> 
            let t = disk_thread () >>= fun _ -> failwith "FIXME we have to allocate from min_free_blk_id" in
            (* FIXME add min_free_elt to freelist state *)
            async t) >>= fun () ->

      (* then try to get a free elt *)
      with_freelist.with_state (fun ~state:s ~set_state -> 
        match s.transient with
        | [] -> (
            (* FIXME are we guaranteed that the disk thread is active? not necessarily *)
            (* assert(s.disk_thread_active=true); *)
            (* we need to have an invariant: after the disk thread
               runs and frees all the waiting threads, then the
               remaining transient elts are at least tr_lower *)
            ev_create () >>= fun (ev: elt Event.event) ->              
            set_state {s with waiting=ev::s.waiting } >>= fun () -> 
            (* NOTE do we have to release the state before waiting?
               yes; and we assume that if an event is fulfilled before
               waiting on it, the wait succeeds ie events are one-shot *)
            return (`Ev ev))
        | elt::transient -> 
          set_state {s with transient} >>= fun () ->
          return (`Elt elt)) >>= fun x ->

      (match x with
       | `Ev ev -> ev_wait ev
       | `Elt elt -> return elt)      
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
      | `Tl_only -> plist.sync_tl ()
      | `Tl_and_root_block -> 
        plist.sync_tl () >>= fun () ->
        plist.get_hd () >>= fun hd ->
        plist.get_tl () >>= fun tl ->
        root_block.write_freelist_roots ~hd ~tl >>= fun () ->
        root_block.sync ()
    in

    { alloc; alloc_many; free; free_many; sync }

end
