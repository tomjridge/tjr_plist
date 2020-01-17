(** We store the list of elts from offset off0 (say); the nxt pointer
   takes up the first off0 bytes. Assumes max size of marshalled blk_ptr
   is off0 bytes *)
open Plist_intf

[@@@warning "-26"] (* FIXME *)

type ('a,'blk_id,'blk,'buf) marshal_info = {
  max_elt_sz    :int;
  (** max size of a marshalled elt option *)
  max_blk_id_sz :int;
  (** max size of a marshalled blk_id option *)
  m_elt         : 'a option -> 'buf*int -> 'buf*int;
  u_elt         : 'buf -> int -> 'a option * int;
  (** None denotes end of list *)
  m_blk_id      : 'blk_id option -> 'buf*int -> 'buf*int;
  u_blk_id      : 'buf -> int -> 'blk_id option*int;
  blk_to_buf: 'blk -> 'buf;
  buf_to_blk: 'buf -> 'blk
  
}

module Make(S:sig 
    type buf
    type blk_id
    type blk
    type t
  end)
= struct
  open S
  let make = fun 
    ~monad_ops
    ~(buf_ops:buf buf_ops)
    ~(blk_ops:blk blk_ops)
    ~(blk_dev_ops:(blk_id,blk,t)blk_dev_ops)
    ~marshal_info
    ->
      let ( >>= ) = monad_ops.bind in
      let return = monad_ops.return in
      let { create;get;len} = buf_ops in
      let { blk_sz;of_bytes;to_bytes;of_string;_ } = blk_ops in
      let { write; _ } = blk_dev_ops in
      let { max_elt_sz; max_blk_id_sz; m_elt; u_elt; 
            m_blk_id; u_blk_id; blk_to_buf; buf_to_blk } = marshal_info in
      let buf_sz = Blk_sz.to_int blk_sz in
      let buf_space ~off = buf_sz - off in
      let can_fit ~off ~n = off+n<=buf_sz in
      
      let elts_offset0 = max_blk_id_sz in
      assert(can_fit ~off:elts_offset0 ~n:(2*max_elt_sz)); 
      (* can fit 2 elts at least - including the None end of list marker *)

      let empty_blk () = String.make buf_sz chr0 |> of_string in

      (* NOTE returns the offset pointing to the None end-of-list marker *)
      let x_to_buf (elts,nxt) : buf*int = 
        (* write nxt *)
        create buf_sz |> fun buf ->
        (buf,0) |> m_blk_id nxt |> fun (buf,i) -> 
        assert(i<=max_blk_id_sz);
        (buf,elts_offset0,elts) |> iter_k (fun ~k (buf,off,elts) ->
            assert(can_fit ~off ~n:max_elt_sz);
            match elts with 
            | [] -> 
              (* note the use of the offset just before the None *)
              (buf,off) |> m_elt None |> fun (buf,_) -> (buf,off)
            | e::elts -> (
                (buf,off) |> m_elt (Some e) |> fun (buf,off) -> 
                k (buf,off,elts)))
      in
      
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

      let ops1 = { unmarshal=blk_to_x; marshal=x_to_blk } in

      let create_plist blk_id = 
        x_to_buf ([],None) |> fun (buf,off) -> 
        let pl = { hd=blk_id;
                   tl=blk_id;
                   buffer=buf;
                   off;
                   nxt_is_none=true;
                   dirty=true;
                 }
        in
        let blk = buf_to_blk buf in
        write ~blk_id ~blk >>= fun () ->
        return {pl with dirty=false}
      in

      (* working with_state *)

      let clear_nxt_blk blk_id = 
        x_to_blk ([],None) |> fun blk ->
        write ~blk_id ~blk
      in

      (* invariant: if tl not written to disk yet, then tl_dirty is true *)
      let sync_tl ~state = 
        let { tl; buffer; dirty; _ } = state in
        match dirty with
          | true -> (
              write ~blk_id:tl ~blk:(buf_to_blk buffer) >>= fun () ->
              return {state with dirty=false })
          | false -> return state
      in

      (* add element in tl; don't use blk_dev *)
      let add_elt ~state ~elt = 
        let buf,off = state.buffer,state.off in
        assert(can_fit ~off ~n:(2*max_elt_sz));
        (* write an elt followed by a None, and update the pointer offset *)
        (buf,off) |> m_elt (Some elt) |> fun (buf,off) -> 
        (buf,off) |> m_elt None |> fun (buf,_) -> 
        (* note the offset is just before the none; FIXME maybe can
           optimize by only writing None once per blk *)
        {state with buffer=buf; off; dirty=true}
      in

      (* don't use blk_dev *)
      let set_nxt nxt buf =
        (* update nxt first *)
        (buf,0) |> m_blk_id nxt |> fun (buf,_) -> 
        buf
      in        

      (* don't use blk_dev *)
      let move_to_nxt ~hd ~nxt ~buf ~off ~dirty =
        (* assert(buf_to_x state.buffer |> snd = nxt); *)
        { hd; tl=nxt; buffer=buf; off; nxt_is_none=true; dirty }
      in

      let add = 
        fun ~(with_state:(('a,blk_id,buf)plist,t) with_state) ->
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
                write ~blk_id ~blk:(state.buffer |> set_nxt (Some nxt) |> buf_to_blk) >>= fun () ->
                move_to_nxt ~hd:state.hd ~nxt ~buf ~off ~dirty:false |> fun state ->
                set_state state >>= fun () ->
                return None))
      in
      let ret = ops1,create_plist,fun with_state -> add ~with_state in
      ret

  let _ : monad_ops:t monad_ops ->
buf_ops:buf buf_ops ->
blk_ops:blk blk_ops ->
blk_dev_ops:(blk_id, blk, t) blk_dev_ops ->
marshal_info:('b, blk_id, blk, buf) marshal_info ->
('b, blk_id, blk) plist_marshal_ops *
(blk_id -> (('c, blk_id, buf) plist, t) m) *
((('a, blk_id, buf) plist, t) with_state ->
 nxt:blk_id -> elt:'b -> (blk_id option, t) m) = make
end

