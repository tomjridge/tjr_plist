(* FIXME perhaps this would be better as a queue of blk_ids, and a
   self-throttling thread on one end *)

open Plist_intf
open Freelist_intf


module type S = sig

  type blk_id
  type blk
  type buf

  type elt (** elements stored in the free list *)

  type t

end

module type T = sig
  include S
      
  (* $(PIPE2SH("""sed -n '/val[ ]make:/,/freelist_ops/p' >GEN.fl_make_1.make.ml_""")) *)
  val make: 
    monad_ops     :(t monad_ops) ->     
    event_ops     :t event_ops ->
    async         :((unit -> (unit, t) m) -> (unit, t) m) ->
    plist_ops     :(elt, buf, blk_id, t) plist_ops ->
    with_freelist :(elt freelist, t) with_state ->
    root_block    :(elt, blk_id, t) fl_root_ops ->
    version       :(elt,blk_id,t)version -> 
    (elt, t) freelist_ops

end




(* [@@@warning "-26-8"] (\* FIXME *\) *)

module Make(S:S) 
  :  (T with type blk_id=S.blk_id and type blk=S.blk and type buf=S.buf and type elt=S.elt and type t=S.t) 
= struct

  include S

  type nonrec version = (elt,blk_id,t)version

  (* try to keep the transient list size between these two *)
  let tr_upper = 4
  let tr_lower = 2

  (* NOTE we need a version which deals with blk_ids, and a version which deals with arbitrary elts *)

  let make 
      ~monad_ops
      ~event_ops
      ~(async:(unit -> (unit,t)m) -> (unit,t)m)
      ~(plist_ops:(elt,buf,blk_id,t)plist_ops)
      ~(with_freelist:(elt freelist,t)with_state)
      ~(root_block:(elt,blk_id,t)fl_root_ops)
      ~(version:version)
    = 
    (* let For_arbitrary_elts blk_alloc = version in *)
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let { ev_create; ev_wait; ev_signal } = event_ops in
    let {add; _ } = plist_ops in

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
        plist_ops.adv_hd () >>= function
        | Error () -> return `End_of_plist 
        | Ok {old_hd; old_elts; _ } -> (
            plist_ops.sync_tl () >>= fun () ->

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
            let t () = 
              disk_thread () >>= fun x -> begin
                match x with 
                | `Finished -> return ()
                | `Unfinished -> 
                  Printf.printf "Disk thread did not allocate; now attempting to use min_free\n%!";
                  with_freelist.with_state (fun ~state ~set_state -> 
                      match state.min_free with
                      | None -> failwith "FIXME at this point there are no free elements"
                      | Some (elt,{min_free_alloc}) -> 
                        (* put 1000+|waiting| free elts into transient *)
                        let num_to_alloc = 1000+List.length state.waiting in
                        Printf.printf "%s: num_to_alloc=%d\n%!" __FILE__ num_to_alloc;
                        min_free_alloc elt num_to_alloc |> fun (elts,elt) -> 
                        (* FIXME as well as set_state, we should record
                           the new min_free elt on disk synchronously
                           otherwise there is a danger that we crash and
                           then reallocate some elts that have already
                           been allocated *)
                        Printf.printf "post disk_thread: adding new transients from min_free\n%!"; 
                        (* remember to wake up any waiting *)
                        (* also remember to use state.transient first (which may be nonempty *)
                        (state.transient@elts,state.waiting) |> iter_k (fun ~k (elts,waiting) ->
                            match elts,waiting with
                            | _,[] -> return (elts,[])
                            | [],_ -> failwith "we need to allocate even more! FIXME"
                            | e::elts,w::waiting -> 
                              ev_signal w e >>= fun () ->
                              k (elts,waiting)) >>= fun (elts,waiting) -> 
                        Printf.printf "post disk_thread: setting state\n%!";
                        set_state { transient=elts; (* FIXME transient=[]? no, just < tr_lower *)
                                    waiting;
                                    disk_thread_active=false;
                                    (* FIXME it clearer why this has to be here - the disk_thread finished long ago *)
                                    min_free=Some(elt,{min_free_alloc}) }
                    )
              end
            in
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
              Printf.printf "%s: thread waits for transient free elt\n%!" __FILE__;
              ev_create () >>= fun (ev: elt event) ->              
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
                  plist_ops.add_if_room e >>= function
                  | true -> return ()
                  | false -> (
                      (* so use this e as the nxt blk *)
                      plist_ops.adv_tl (e2b e)))
              | x1::x2::xs -> (
                  plist_ops.add ~nxt:(e2b x2) ~elt:x1 >>= function
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
      | `Tl_only -> plist_ops.sync_tl ()
      | `Tl_and_root_block -> 
        plist_ops.sync_tl () >>= fun () ->
        plist_ops.get_hd () >>= fun hd ->
        plist_ops.get_tl () >>= fun tl ->
        plist_ops.blk_len () >>= fun blk_len ->
        root_block.write_root {hd;tl;blk_len;min_free=failwith "FIXME"} >>= fun () ->
        root_block.sync ()
    in

    { alloc; alloc_many; free; free_many; sync }

end

(** Make without functor *)
let make (type blk_id blk buf elt t) x : (elt,t)freelist_ops = 
  let module S = struct
    type nonrec blk_id = blk_id
    type nonrec blk = blk
    type nonrec buf = buf
    type nonrec elt = elt
    type nonrec t = t
  end
  in
  let open (Make(S)) in
  make ~monad_ops:(x#monad_ops)
    ~event_ops:(x#event_ops)
    ~async:(x#async)
    ~plist_ops:(x#plist_ops)
    ~with_freelist:(x#with_freelist)
    ~root_block:(x#root_block)
    ~version:(x#version)


let _ = make

(*
  
  let _ :
monad_ops:t Tjr_monad.monad_ops ->
event_ops:t Tjr_monad.event_ops ->
async:((unit -> (unit, t) Tjr_monad.m) -> (unit, t) Tjr_monad.m) ->
plist:(elt, buf, blk_id, t) Tjr_plist.Plist_intf.plist_ops ->
with_freelist:(elt Tjr_plist_freelist__.Freelist_intf.freelist, t)
              Tjr_monad.with_state ->
root_block:(elt, blk_id, t) Tjr_plist_freelist__.Freelist_intf.fl_root_ops ->
version:version -> (elt, t) Tjr_plist_freelist__.Freelist_intf.freelist_ops
= make

(**/**)
let make (type blk_id blk buf elt t) x : (elt,t)freelist_ops = 
  let module S = struct
    type nonrec blk_id = blk_id
    type nonrec blk = blk
    type nonrec buf = buf
    type nonrec elt = elt
    type nonrec t = t
  end
  in
  let open (Make(S)) in
  make ~monad_ops:(x#monad_ops)
    ~event_ops:(x#event_ops)
    ~async:(x#async)
    ~plist:(x#plist)
    ~with_freelist:(x#with_freelist)
    ~root_block:(x#root_block)
    ~version:(x#version)
(**/**)

let make : 
< async : (unit -> (unit, 't) m) -> (unit, 't) m;
  event_ops : 't event_ops;
  monad_ops : 't monad_ops;
  plist : ('elt, 'buf, 'blk_id, 't) plist_ops;
  root_block : ('elt,'blk_id, 't) fl_root_ops;
  version : ('elt, 'blk_id, 't) version;
  with_freelist : ('elt freelist, 't) with_state;
> -> ('elt, 't) freelist_ops
= make
(** 

{[
let make : 
< async : (unit -> (unit, 't) m) -> (unit, 't) m;
  event_ops : 't event_ops;
  monad_ops : 't monad_ops;
  plist : ('elt, 'buf, 'blk_id, 't) plist_ops;
  root_block : ('blk_id, 't) root_block_ops;
  version : ('elt, 'blk_id, 't) version;
  with_freelist : ('elt freelist, 't) with_state;
> -> ('elt, 't) freelist_ops
= make
]}

*)
*)
