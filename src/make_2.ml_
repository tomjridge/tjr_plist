(** We store the list of elts from offset off0 (say); the nxt pointer
   takes up the first off0 bytes. Assumes max size of marshalled
   blk_ptr is off0 bytes.

    When an elt is added, we marshal to buffer immediately (same for
   nxt pointer). Only when we write to disk do we update the buffer
   with the None pointer.


  *)
open Plist_intf

[@@@warning "-26"] (* FIXME *)

module Make(S:sig 
    type buf
    type blk_id
    type blk
      
    type elt
    val plist_marshal_info: (elt, blk_id, blk, buf) plist_marshal_info
    val buf_ops: buf buf_ops
    val blk_ops: blk blk_ops
  end)
= struct
  include S
      
  let { create;get;len; _} = buf_ops 
  let { blk_sz;of_bytes;to_bytes;of_string;_ } = blk_ops
  let { max_elt_sz; max_blk_id_sz; m_elt; u_elt; 
        m_blk_id; u_blk_id; blk_to_buf; buf_to_blk } = plist_marshal_info
  let buf_sz = Blk_sz.to_int blk_sz 
  let buf_space ~off = buf_sz - off 
  let can_fit ~off ~n = off+n<=buf_sz 

  let elts_offset0 = max_blk_id_sz

  let _ = 
    assert(blk_sz = blk_sz_4096);
    assert(can_fit ~off:elts_offset0 ~n:(2*max_elt_sz))
  (* can fit 2 elts at least - including the None end of list marker *)

  (* we need to fit an elt and a None list terminator *)
  let can_fit_elt off = can_fit ~off ~n:(2*max_elt_sz) 


  let empty_blk () = String.make buf_sz chr0 |> of_string 

  (* NOTE returns the offset pointing to the None end-of-list marker *)
  let x_to_buf (elts,nxt) : buf*int = 
    (* write nxt *)
    create buf_sz |> fun buf ->
    (buf,0) |> m_blk_id nxt |> fun (buf,i) -> 
    assert(i<=max_blk_id_sz);
    (buf,elts_offset0,elts) |> iter_k (fun ~k (buf,off,elts) ->
        assert(can_fit_elt off);
        match elts with 
        | [] -> 
          (* note the use of the offset just before the None *)
          (buf,off) |> m_elt None |> fun (buf,_) -> (buf,off)
        | e::elts -> (
            (buf,off) |> m_elt (Some e) |> fun (buf,off) -> 
            k (buf,off,elts)))

  (* FIXME this assumes that the buf has a trailing None *)
  let buf_to_x buf = 
    0 |> u_blk_id buf |> fun (nxt,_) -> 
    (elts_offset0,[]) |> iter_k (fun ~k (off,acc) -> 
        u_elt buf off |> fun (e,off) -> 
        match e with 
        | None -> (List.rev acc,off)  (* note return pointer to None *)
        | Some e -> k (off,e::acc))
    |> fun (es,off) -> 
    es,nxt

  let x_to_blk (elts,nxt) = 
    x_to_buf (elts,nxt) |> fun (buf,_) -> buf_to_blk buf

  let blk_to_x blk = blk_to_buf blk |> buf_to_x 

  let m_u_ops = {marshal=x_to_blk; unmarshal=blk_to_x } 

  module Make__mops_blk(
      X1: sig
        type t
        val monad_ops: t monad_ops
        val blk_dev_ops:(blk_id,blk,t)blk_dev_ops
      end)
  =
  struct
    open X1
    let ( >>= ) = monad_ops.bind 
    let return = monad_ops.return
    let { write; read; _ } = blk_dev_ops

    let create_plist blk_id = 
      x_to_buf ([],None) |> fun (buf,off) -> 
      let pl = { hd=blk_id;
                 tl=blk_id;
                 blk_len=1;
                 buffer=buf;
                 off;
                 (* nxt_is_none=true; *)
                 dirty=true;
               }
      in
      let blk = buf_to_blk buf in
      write ~blk_id ~blk >>= fun () ->
      return {pl with dirty=false}

    (* FIXME rename to eg read_entire_plist *)
    let read_plist blk_id = 
      (blk_id,[]) |> iter_k (fun ~k (blk_id,acc) -> 
          read ~blk_id >>= fun blk ->
          blk_to_x blk |> fun (elts,nxt) ->
          match nxt with 
          | None -> 
            let acc = (elts,nxt)::acc in
            return (List.rev acc)
          | Some nxt -> k (nxt,(elts,Some nxt)::acc))

    (* FIXME rename to eg create_plist_from_tl *)
    let read_plist_tl ~hd ~tl ~blk_len = 
      read ~blk_id:tl >>= fun blk ->
      blk_to_x blk |> fun (elts,nxt) ->
      (elts,nxt) |> x_to_buf |> fun (buf,off) ->                  
      return { hd; tl; buffer=buf; off; blk_len; dirty=false }


    let extra_ops = { create_plist; read_plist; read_plist_tl }

    module Make__wst(X2:sig val with_state: ((elt,blk_id,buf)plist,t) with_state end)
    = struct
      open X2              
      (* working with_state *)

      let clear_nxt_blk blk_id = 
        x_to_blk ([],None) |> fun blk ->
        write ~blk_id ~blk

      let add_elt_list_terminator off buf = 
        (buf,off) |> m_elt None |> fun (buf,_) -> 
        buf

      (* invariant: if tl not written to disk yet, then tl_dirty is true *)
      let sync' ~state = 
        let { tl; buffer=buf; dirty; off; _ } = state in
        match dirty with
        | true -> (
            (* marshal None at off *)
            add_elt_list_terminator off buf |> fun buf ->
            write ~blk_id:tl ~blk:(buf_to_blk buf) >>= fun () ->
            return {state with dirty=false })
        | false -> return state

      (* add element in tl; don't use blk_dev *)
      let add_elt ~state ~elt = 
        let buf,off = state.buffer,state.off in
        assert(can_fit_elt off);
        (* write an elt and update the pointer offset *)
        (buf,off) |> m_elt (Some elt) |> fun (buf,off) -> 
        {state with buffer=buf; off; dirty=true}

      (* don't use blk_dev *)
      let set_nxt nxt buf =
        (* update nxt first *)
        (buf,0) |> m_blk_id nxt |> fun (buf,_) -> 
        buf

      (* don't use blk_dev *)
      let move_to_nxt ~hd ~nxt ~blk_len ~buf ~off ~dirty =
        (* assert(buf_to_x state.buffer |> snd = nxt); *)
        { hd; tl=nxt; blk_len; buffer=buf; off; (* nxt_is_none=true; *) dirty }

      let add_if_room elt = 
        with_state.with_state (fun ~state ~set_state ->
            let { off; _ } = state in
            match can_fit_elt off with
            | true -> add_elt ~state ~elt |> fun s ->
                      set_state s >>= fun () ->
                      return true
            | false -> return false)

      let add = 
        fun ~nxt ~elt ->
        with_state.with_state (fun ~state ~set_state ->
            let { off; _ } = state in
            match can_fit ~off ~n:(2*max_elt_sz) with
            | true -> (
                add_elt ~state ~elt |> fun state ->
                set_state state >>= fun () ->
                return (Some nxt))
            | false -> (
                (* write the new blk first *)
                x_to_buf ([elt],None) |> fun (buf,off) ->
                write ~blk_id:nxt ~blk:(buf_to_blk buf) >>= fun () ->
                (* write old blk with nxt *)
                let blk_id = state.tl in
                let buf' = state.buffer |> set_nxt (Some nxt)
                           |> add_elt_list_terminator state.off
                in
                write ~blk_id ~blk:(buf' |> buf_to_blk) >>= fun () ->
                move_to_nxt ~hd:state.hd ~nxt ~blk_len:(state.blk_len+1) ~buf ~off ~dirty:false 
                |> set_state >>= fun () ->
                return None))

      let sync_tl =
        fun () ->
        with_state.with_state (fun ~state ~set_state ->
            let { tl=blk_id; buffer=buf; off; dirty; _ } = state in
            buf |> add_elt_list_terminator off |> fun buf ->
            write ~blk_id ~blk:(buf_to_blk buf) >>= fun () ->
            set_state {state with buffer=buf; dirty=false})

      let blk_len () = 
        with_state.with_state (fun ~state ~set_state ->
            return state.blk_len)

      let adv_hd () = 
        with_state.with_state (fun ~state ~set_state ->
            let { hd; _ } = state in
            read ~blk_id:hd >>= fun old_blk ->
            old_blk |> blk_to_x |> fun (elts,nxt) ->        
            match nxt with
            | None -> return (Error ())
            | Some new_hd -> 
              set_state { state with hd=new_hd;blk_len=state.blk_len-1 } >>= fun () ->
              return (Ok { old_hd=hd; old_elts=elts; new_hd }))

      let adv_tl nxt = 
        with_state.with_state (fun ~state ~set_state ->
            (* write the new blk first *)
            x_to_buf ([],None) |> fun (buf,off) ->
            write ~blk_id:nxt ~blk:(buf_to_blk buf) >>= fun () ->
            (* write old blk with nxt *)
            let blk_id = state.tl in
            let buf' = state.buffer |> set_nxt (Some nxt)
                       |> add_elt_list_terminator state.off
            in
            write ~blk_id ~blk:(buf' |> buf_to_blk) >>= fun () ->
            move_to_nxt ~hd:state.hd ~nxt ~blk_len:(state.blk_len+1) ~buf ~off ~dirty:false
            |> set_state >>= fun () ->
            return ())

      let get_hd () = 
        with_state.with_state (fun ~state ~set_state ->
            return (state.hd))

      let get_tl () = 
        with_state.with_state (fun ~state ~set_state ->
            return (state.tl))

      let get_hd_tl () = 
        with_state.with_state (fun ~state ~set_state ->
            return (state.hd,state.tl))

      let read_blk blk_id = 
        read ~blk_id >>= fun blk ->
        try 
          let x = blk_to_x blk in
          return (Ok(blk_to_x blk))
        with _ -> return (Error ())

      let read_hd () =
        get_hd () >>= fun hd -> 
        get_tl () >>= fun tl ->  (* FIXME only needed for checking *)
        (* FIXME following should be a warning on log *)
        assert((match hd=tl with | true -> Printf.printf "Warning: read_hd, attempt to read hd when hd=tl; hd may not be synced" | false -> ()); true);
        read_blk hd >>= fun r ->
        match r with
        | Ok x -> return x
        | Error () -> failwith "read_hd: hd block was not marshalled correctly on disk; are you sure it was synced?"

      let append pl2 = 
        with_state.with_state (fun ~state ~set_state -> 
            let buf = state.buffer |> set_nxt (Some(pl2.hd)) in
            write ~blk_id:state.tl ~blk:(buf_to_blk buf) >>= fun () ->
            let { hd; tl; buffer; off; blk_len; dirty=_ } = state in
            set_state { hd; tl=pl2.tl; buffer=pl2.buffer; off=pl2.off; 
                        blk_len=(blk_len+pl2.blk_len); dirty=pl2.dirty })


      let plist_ops = 
        { add;add_if_room;sync_tl;blk_len;adv_hd;adv_tl;get_hd;get_tl;get_hd_tl;read_hd;append }
    end
  end
end

module type R = sig
  type buf
  type blk_id
  type blk
  type elt

  val empty_blk : unit -> blk
  val m_u_ops : (elt, blk_id, blk) plist_marshal_ops
  module Make__mops_blk :
    functor
      (X1 : sig
         type t
         val monad_ops : t monad_ops
         val blk_dev_ops : (blk_id, blk, t) blk_dev_ops
       end) ->
    sig
      val create_plist : blk_id -> (('a, blk_id, buf) plist, X1.t) m
      val read_plist :
        blk_id -> ((elt list * blk_id option) list, X1.t) m
      val read_plist_tl :
        hd:blk_id ->
        tl:blk_id -> blk_len:int -> (('a, blk_id, buf) plist, X1.t) m
      val extra_ops : (elt, buf, blk_id, X1.t) plist_extra_ops
      module Make__wst :
        functor
          (X2 : sig
             val with_state :
               ((elt, blk_id, buf) plist, X1.t) with_state
           end) ->
        sig
          val plist_ops : (elt, buf, blk_id, 'a, X1.t) plist_ops
        end
    end
end


let make_1 (type buf blk_id blk elt) ~buf_ops ~blk_ops ~plist_marshal_info = 
  let module X1 = struct 
    type nonrec buf=buf
    type nonrec blk_id=blk_id
    type nonrec blk=blk
    type nonrec elt=elt
    let buf_ops = buf_ops let blk_ops = blk_ops let plist_marshal_info = plist_marshal_info end 
  in
  let module X2 = Make(X1) in
  let module X3 : R with type buf=buf and type blk_id=blk_id and type blk=blk and type elt=elt  = X2 in
  (module X3 : (R with type buf=buf and type blk_id=blk_id and type blk=blk and type elt=elt))

let _ : buf_ops:'a buf_ops ->
blk_ops:'b blk_ops ->
plist_marshal_info:('c, 'd, 'b, 'a) plist_marshal_info ->
(module R with type blk = 'b and type blk_id = 'd and type buf = 'a and type elt = 'c) = make_1

(*


(**/**)
let make (type buf blk_id blk t) ~plist_marshal_info = 
  let module S = struct
    type nonrec buf = buf
    type nonrec blk_id = blk_id
    type nonrec blk = blk
    type nonrec t = t
  end
  in
  let module T = Make(S) in
  T.make ~plist_marshal_info
(**/**)

let make ~(plist_marshal_info:('a, 'blk_id, 'blk, 'buf) plist_marshal_info) = make ~plist_marshal_info

let _ = make

*)
