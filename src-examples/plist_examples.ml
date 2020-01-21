open Tjr_plist
open Plist_intf

type blk_id = Blk_id_as_int.blk_id

let monad_ops = Tjr_monad.lwt_monad_ops

let ( >>= ) = monad_ops.bind

let return = monad_ops.return


module Blk_as_bytes_buf_as_bytes = struct
  type blk = bytes (* NOTE we must take care to copy the bytes when writing to disk *)

  (* FIXME rename Common_blk_ops so it starts with Blk_; also
     blk_as_string rather than string_blk_ops *)
  let blk_ops = Tjr_fs_shared.Common_blk_ops.bytes_blk_ops

  type buf = Buf_as_bytes.by_buf


  let make = make

  let buf_ops = Buf_as_bytes.by_buf_ops

  let blk_dev_ops with_map = 
    Blk_dev_factory.(make A3_bytes_4096_lwt_mem
                     |> function | (R3 k) -> k with_map
                                 | _ -> failwith __LOC__)

  let m_any max_sz (x:'a) (buf,off) = 
    (* Printf.printf "Buffer size: %d; off:%d\n" (Bytes.length buf) off; *)
    let n : int = Marshal.to_buffer buf off max_sz x [] in 
    (buf,off+n)

  let u_any buf off : 'a * int = 
    (* Printf.printf "Reading at offset %d\n" off; *)
    let i : 'a = Marshal.from_bytes buf off in
    (* FIXME Marshal should have a from_buffer which returns value and
       number of bytes consumed *)
    let n = Marshal.total_size buf off in
    (i,off+n)


  module Int_plist = struct
    let max_sz = 32

    (* FIXME these max sizes are guessed upper bounds *)
    (* TODO we also need a version with bin_prot marshalling, for size
       and speed *)
    let marshal_info : (int,blk_id,blk,buf)plist_marshal_info = {
      max_elt_sz=max_sz;
      max_blk_id_sz=max_sz;
      m_elt=m_any max_sz;
      u_elt=u_any;
      m_blk_id=m_any max_sz;
      u_blk_id=u_any;
      blk_to_buf=(fun x -> x);
      buf_to_blk=(fun x -> Bytes.copy x);
    }

    (* NOTE this gets filled in later, once we have the blk/buf
       marshalling function *)
    (* let check_blk = ref (fun buf -> true) *)

    (* let write ~blk_id ~blk = 
     *   assert(!check_blk blk);
     *   blk_dev_ops.write ~blk_id ~blk
     * in
     * let read ~blk_id = 
     *   blk_dev_ops.read ~blk_id >>= fun blk ->
     *   assert(!check_blk blk);
     *   return blk
     * in
     * let blk_dev_ops = { blk_dev_ops with read; write } in *)

    let (plist_marshal_ops,rest) = 
      make ~plist_marshal_info:marshal_info ~buf_ops ~blk_ops |> fun (plist_marshal_ops, `K1 f) ->
      (plist_marshal_ops,f)

  end

  module Test() : sig
    val main : unit -> (unit, lwt) m
  end = struct
    open Int_plist

    (** In-mem blk dev, mutated directly *)
    let mem = ref (Tjr_map.With_pervasives_compare.empty ())

    let with_mem : ('a,lwt)with_state = 
      let with_state f = f ~state:!mem ~set_state:(fun s -> mem:=s; return ()) in
      { with_state }

    let _ = with_mem

    let blk_dev_ops = blk_dev_ops with_mem

    let plist_extra_ops,`K2 plist_ops = rest ~monad_ops ~blk_dev_ops

    let {create_plist;read_plist} = plist_extra_ops

    (* let _ = 
     *   Int_plist.check_blk := (fun blk -> let _ = m_u_ops.unmarshal blk in true) *)

    let num_elts = 10_000

    let main () = 
      create_plist (Blk_id_as_int.of_int 0) >>= fun plist -> 
      let state = ref plist in
      let with_state : ((int, blk_id, buf) plist,lwt) with_state = 
        let with_state f = f ~state:!state ~set_state:(fun s -> 
            state:=s; return ())
        in
        {with_state}
      in
      let {add;sync} = plist_ops ~with_state in
      (1,0) |> iter_k (fun ~k (min_free_blk_id,n) -> 
          match n >= num_elts with
          | true -> 
            (* NOTE make sure the last block is actually written *)
            sync () >>= fun () ->
            return min_free_blk_id
          | false -> 
            add
              ~nxt:(Blk_id_as_int.of_int min_free_blk_id)
              ~elt:n
            >>= fun x -> 
            match x with 
            | None -> k (min_free_blk_id+1,n+1)
            | Some _ -> k (min_free_blk_id,n+1))
      >>= fun min_free_blk_id ->     
      Printf.printf "Finished with next free blk: %d\n" min_free_blk_id;
      read_plist (Blk_id_as_int.of_int 0) >>= fun xs ->
      xs |> List.map (fun (elts,nxt_) -> elts) |> List.concat |> fun xs ->
      Printf.printf "Read %d elts from disk\n" (List.length xs);
      assert(num_elts = List.length xs);
      return ()

    let _ = main
  end

end


module Blk_as_ba_buf_as_ba = struct
  type blk = Buf_as_bigarray.ba_buf (* NOTE we must take care to copy the bytes when writing to disk *)

  let blk_ops = Blk_factory.(make A3_ba_4096 |> function R3 ops -> ops | _ -> failwith __LOC__)

  type buf = Buf_as_bigarray.ba_buf

  let buf_ops = Buf_as_bigarray.ba_buf_ops

  module Int_binprot = struct

    module Int_option = struct
      open Bin_prot.Std
      type int_opt = int option [@@deriving bin_io]
    end

    let max_sz = 10 (* 1 for option; 1 for int tag; 8 for int *)

    let m_elt (x:int option) (buf,off) = 
      Int_option.bin_write_int_opt buf ~pos:off x |> fun off' -> 
      (buf,off')

    let m_blk_id blk_id = 
      blk_id |> (function None -> None | Some x -> Some (Blk_id_as_int.to_int x))
      |> m_elt 

    let u_elt buf off = 
      let pos_ref = ref off in
      Int_option.bin_read_int_opt buf ~pos_ref |> fun r ->
      (r,!pos_ref)

    let u_blk_id buf off = 
      u_elt buf off |> fun (x,off) -> 
      x |> (function None -> None | Some x -> Some(Blk_id_as_int.of_int x)) |> fun x -> 
      (x,off)

    let marshal_info : (int,blk_id,blk,buf)plist_marshal_info = {
      max_elt_sz=max_sz;
      max_blk_id_sz=max_sz;
      m_elt;
      u_elt;
      m_blk_id;
      u_blk_id;
      blk_to_buf=(fun x -> Buf_as_bigarray.ba_copy x);
      buf_to_blk=(fun x -> Buf_as_bigarray.ba_copy x);
    }

    let blk_dev_ops with_map = 
      Blk_dev_factory.(make A4_ba_4096_lwt_mem
                       |> function | (R4 k) -> k with_map
                                   | _ -> failwith __LOC__)


    let (plist_marshal_ops,rest) = 
      make ~plist_marshal_info:marshal_info ~buf_ops ~blk_ops |> fun (plist_marshal_ops, `K1 f) ->
      (plist_marshal_ops,f)

  end

  module Test() : sig
    val main : unit -> (unit, lwt) m
  end = struct
    open Int_binprot

    (** In-mem blk dev, mutated directly *)
    let mem = ref (Tjr_map.With_pervasives_compare.empty ())

    let with_mem : ('a,lwt)with_state = 
      let with_state f = f ~state:!mem ~set_state:(fun s -> mem:=s; return ()) in
      { with_state }

    let _ = with_mem

    let blk_dev_ops = blk_dev_ops with_mem

    let plist_extra_ops,`K2 plist_ops = rest ~monad_ops ~blk_dev_ops

    let {create_plist;read_plist} = plist_extra_ops

    (* let _ = 
     *   Int_plist.check_blk := (fun blk -> let _ = m_u_ops.unmarshal blk in true) *)

    let num_elts = 10_000

    let main () = 
      create_plist (Blk_id_as_int.of_int 0) >>= fun plist -> 
      let state = ref plist in
      let with_state : ((int, blk_id, buf) plist,lwt) with_state = 
        let with_state f = f ~state:!state ~set_state:(fun s -> 
            state:=s; return ())
        in
        {with_state}
      in
      let {add;sync} = plist_ops ~with_state in
      (1,0) |> iter_k (fun ~k (min_free_blk_id,n) -> 
          match n >= num_elts with
          | true -> 
            (* NOTE make sure the last block is actually written *)
            sync () >>= fun () ->
            return min_free_blk_id
          | false -> 
            add
              ~nxt:(Blk_id_as_int.of_int min_free_blk_id)
              ~elt:n
            >>= fun x -> 
            match x with 
            | None -> k (min_free_blk_id+1,n+1)
            | Some _ -> k (min_free_blk_id,n+1))
      >>= fun min_free_blk_id ->     
      Printf.printf "Finished with next free blk: %d\n" min_free_blk_id;
      read_plist (Blk_id_as_int.of_int 0) >>= fun xs ->
      xs |> List.map (fun (elts,nxt_) -> elts) |> List.concat |> fun xs ->
      Printf.printf "Read %d elts from disk\n" (List.length xs);
      assert(num_elts = List.length xs);
      return ()

    let _ = main
  end

end

