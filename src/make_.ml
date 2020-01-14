(** We store the list of elts from offset 9 (say); the nxt pointer
   takes up the first 9 bytes. Assumes max size of marshalled blk_ptr
   is 9 bytes *)
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

(** Internal version *)
module Internal = struct

  let make (type buf blk_id blk t) = fun 
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
      
      let elts_offset = max_blk_id_sz in
      assert(elts_offset + max_elt_sz <= buf_sz);        

      let empty_blk () = String.make buf_sz chr0 |> of_string in

      (* NOTE returns the offset pointing to the None end-of-list marker *)
      let x_to_buffer ~elts ~nxt : buf*int = 
        (* write nxt *)
        create buf_sz |> fun buf ->
        (buf,0) |> m_blk_id nxt |> fun (buf,i) -> 
        assert(i<=max_blk_id_sz);
        (buf,elts_offset,elts) |> iter_k (fun ~k (buf,off,elts) ->
            assert(off+max_elt_sz < buf_sz);
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
        (elts_offset,[]) |> iter_k (fun ~k (off,acc) -> 
            u_elt buf off |> fun (e,off) -> 
            match e with 
            | None -> (List.rev acc,off)  (* note return pointer to None *)
            | Some e -> k (off,e::acc))
        |> fun (es,off) -> 
        es,nxt
      in            
        
      let empty blk_id = 
        let elts = [] in
        let tl_nxt = None in
        let tl_buffer = x_to_buffer ~elts ~nxt:tl_nxt in
        let pl = { hd=blk_id;
                   tl=blk_id;
                   tl_contents=elts;
                   tl_buffer;
                   tl_nxt;
                   tl_dirty=false; (* but we must write to disk *)
                 }
        in
        let blk = buf_to_blk (fst tl_buffer) in
        write ~blk_id ~blk >>= fun () ->
        return pl
      in

      let marshal (elts,nxt) = 
        x_to_buffer ~elts ~nxt |> fun (buf,_) -> buf_to_blk buf
      in
      let unmarshal blk = blk_to_buf blk |> buf_to_x in      
      {
        add=failwith "";
        sync_tl=failwith"";
        empty;
        marshal;
        unmarshal
      }

end

