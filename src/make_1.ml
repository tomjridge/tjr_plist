(** Construct a persistent list.

We store the list of elts from offset off0 (say); the nxt pointer
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
    type t
  end)
= struct
  open S
  let make = fun 
    ~plist_marshal_info
    ~(buf_ops:buf buf_ops)
    ~(blk_ops:blk blk_ops)
    -> 
      let { create;get;len; _} = buf_ops in
      let { blk_sz;of_bytes;to_bytes;of_string;_ } = blk_ops in
      let { elt_mshlr; blk_id_mshlr; blk_to_buf; buf_to_blk } = 
        plist_marshal_info in
      let buf_sz = Blk_sz.to_int blk_sz in
      let buf_space ~off = buf_sz - off in
      let can_fit ~off ~n = off+n<=buf_sz in
      
      let max_blk_id_sz = blk_id_mshlr.max_elt_sz in
      let max_elt_sz = elt_mshlr.max_elt_sz in

      let elts_offset0 = max_blk_id_sz in
      assert(blk_sz = blk_sz_4096);
      assert(can_fit ~off:elts_offset0 ~n:(2*max_elt_sz)); 
      (* can fit 2 elts at least - including the None end of list marker *)

      (* we need to fit an elt and a None list terminator *)
      let can_fit_elt off = can_fit ~off ~n:(2*max_elt_sz) in


      let empty_blk () = String.make buf_sz chr0 |> of_string in

      let m_blk_id = blk_id_mshlr.mshl in
      let u_blk_id = blk_id_mshlr.umshl in
      let m_elt = elt_mshlr.mshl in
      let u_elt = elt_mshlr.umshl in
      

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
      in

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
      in

      let x_to_blk (elts,nxt) = 
        x_to_buf (elts,nxt) |> fun (buf,_) -> buf_to_blk buf
      in
      let blk_to_x blk = blk_to_buf blk |> buf_to_x in      

      let m_u_ops = {marshal=x_to_blk; unmarshal=blk_to_x } in

      (m_u_ops,`K1 (
           fun 
             ~monad_ops
             ~(blk_dev_ops:(blk_id,blk,t)blk_dev_ops)
             ->
               let ( >>= ) = monad_ops.bind in
               let return = monad_ops.return in
               let { write; read; _ } = blk_dev_ops in

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
               in
               
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
               in

               (* FIXME rename to eg create_plist_from_tl *)
               let read_plist_tl ~hd ~tl ~blk_len = 
                 read ~blk_id:tl >>= fun blk ->
                 blk_to_x blk |> fun (elts,nxt) ->
                 (elts,nxt) |> x_to_buf |> fun (buf,off) ->                  
                 return { hd; tl; buffer=buf; off; blk_len; dirty=false }
               in
                 

               let extra_ops = { create_plist; read_plist; read_plist_tl } in

               (extra_ops, `K2(
                    fun ~(with_state:((blk_id,buf)plist,t) with_state) -> 

                      (* working with_state *)

                      let clear_nxt_blk blk_id = 
                        x_to_blk ([],None) |> fun blk ->
                        write ~blk_id ~blk
                      in

                      let add_elt_list_terminator off buf = 
                        (buf,off) |> m_elt None |> fun (buf,_) -> 
                        buf
                      in

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
                      in

                      (* add element in tl; don't use blk_dev *)
                      let add_elt ~state ~elt = 
                        let buf,off = state.buffer,state.off in
                        assert(can_fit_elt off);
                        (* write an elt and update the pointer offset *)
                        (buf,off) |> m_elt (Some elt) |> fun (buf,off) -> 
                        {state with buffer=buf; off; dirty=true}
                      in

                      (* don't use blk_dev *)
                      let set_nxt nxt buf =
                        (* update nxt first *)
                        (buf,0) |> m_blk_id nxt |> fun (buf,_) -> 
                        buf
                      in

                      (* don't use blk_dev *)
                      let move_to_nxt ~hd ~nxt ~blk_len ~buf ~off ~dirty =
                        (* assert(buf_to_x state.buffer |> snd = nxt); *)
                        { hd; tl=nxt; blk_len; buffer=buf; off; (* nxt_is_none=true; *) dirty }
                      in

                      let add_if_room elt = 
                        with_state.with_state (fun ~state ~set_state ->
                          let { off; _ } = state in
                          match can_fit_elt off with
                          | true -> add_elt ~state ~elt |> fun s ->
                                    set_state s >>= fun () ->
                                    return true
                          | false -> return false)
                      in
                        
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
                      in
                      let sync_tl =
                        fun () ->
                          with_state.with_state (fun ~state ~set_state ->
                            let { tl=blk_id; buffer=buf; off; dirty; _ } = state in
                            buf |> add_elt_list_terminator off |> fun buf ->
                            write ~blk_id ~blk:(buf_to_blk buf) >>= fun () ->
                            set_state {state with buffer=buf; dirty=false})
                      in
                      let blk_len () = 
                        with_state.with_state (fun ~state ~set_state ->
                          return state.blk_len)
                      in
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
                      in
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
                      in
                      let get_hd () = 
                        with_state.with_state (fun ~state ~set_state ->
                          return (state.hd))
                      in
                      let get_tl () = 
                        with_state.with_state (fun ~state ~set_state ->
                          return (state.tl))
                      in
                      let get_hd_tl () = 
                        with_state.with_state (fun ~state ~set_state ->
                          return (state.hd,state.tl))
                      in
                      let read_blk blk_id = 
                        read ~blk_id >>= fun blk ->
                        try 
                          let x = blk_to_x blk in
                          return (Ok(blk_to_x blk))
                        with _ -> return (Error ())
                      in                            
                      let read_hd () =
                        get_hd () >>= fun hd -> 
                        get_tl () >>= fun tl ->  (* FIXME only needed for checking *)
                        (* FIXME following should be a warning on log *)
                        assert((match hd=tl with | true -> Printf.printf "Warning: read_hd, attempt to read hd when hd=tl; hd may not be synced" | false -> ()); true);
                        read_blk hd >>= fun r ->
                        match r with
                        | Ok x -> return x
                        | Error () -> failwith "read_hd: hd block was not marshalled correctly on disk; are you sure it was synced?"
                      in
                      let append pl2 = 
                        with_state.with_state (fun ~state ~set_state -> 
                          let buf = state.buffer |> set_nxt (Some(pl2.hd)) in
                          write ~blk_id:state.tl ~blk:(buf_to_blk buf) >>= fun () ->
                          let { hd; tl; buffer; off; blk_len; dirty=_ } = state in
                          set_state { hd; tl=pl2.tl; buffer=pl2.buffer; off=pl2.off; 
                                      blk_len=(blk_len+pl2.blk_len); dirty=pl2.dirty })
                      in
                      (*
                      let read_tl =
                        get_hd_tl () >>= fun (hd_,tl) ->
                        read_blk tl
                      in
                         *)
                      { add;add_if_room;sync_tl;blk_len;adv_hd;adv_tl;get_hd;get_tl;get_hd_tl;read_hd;append }))))

  let _ = make
end

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

