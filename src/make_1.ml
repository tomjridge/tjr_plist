(** Construct a persistent list (most generic version, not concurrent-safe).

We store the list of elts from offset off0 (say); the nxt pointer
   takes up the first off0 bytes. Assumes max size of marshalled
   blk_ptr is off0 bytes.

    When an elt is added, we marshal to buffer immediately (same for
   nxt pointer). Only when we write to disk do we update the buffer
    with the None pointer.


Plist block structure:

{%html:
<img width='100%' src="https://docs.google.com/drawings/d/e/2PACX-1vS3avt7A5v-_ormHZlUYMIjUtOQ63U5tL4nDgHkIVI8VXqraTu6ClOCmYcMy3HeS5dCT-6LYpAYFgAC/pub?w=1033&amp;h=312">
%}

Some invariants: 

- adding an element, or setting the nxt pointer, marshals immediately
- the EOL marker is only marshalled when syncing the block (presumably to avoid repeated marshalling of this marker)

  *)


open Plist_intf

[@@@warning "-26"] (* FIXME *)

module type S = sig 
  type buf
  type blk_id
  type blk
  type t

  val buf_ops:buf buf_ops
  val blk_ops:blk blk_ops

  type a
  val plist_marshal_info: (a,blk_id,blk,buf)plist_marshal_info
end

module type T = sig
  module S : S
  open S

  val plist_factory : monad_ops:t Tjr_monad.monad_ops ->
    (a, blk_id, blk, buf, t) plist_factory 
end

module Make_v1(S:S) = struct
  module S = S
  open S


  (*****************************************
   * Start with basic marshalling routines *
   *****************************************)

  module A = struct
    let { create;get=_;len=_FIXME; _} = buf_ops 
    let { blk_sz;of_bytes=_;to_bytes=_;of_string;_ } = blk_ops
    let { elt_mshlr; blk_id_mshlr; blk_to_buf; buf_to_blk } = 
      plist_marshal_info 
    let buf_sz = Blk_sz.to_int blk_sz 
    let _buf_space ~off = buf_sz - off 
    let can_fit ~off ~n = off+n<=buf_sz 

    let max_blk_id_sz = blk_id_mshlr.max_elt_sz 
    let max_elt_sz = elt_mshlr.max_elt_sz 

    (* The offset where we start writing elements *)
    let off0 = max_blk_id_sz 

    let _ : unit = 
      assert(blk_sz = blk_sz_4096);
      assert(can_fit ~off:off0 ~n:(2*max_elt_sz)); 
      ()
    (* can fit 2 elts at least - including the None end of list marker *)

    (* we need to fit an elt and a None list terminator *)
    let can_fit_elt off = can_fit ~off ~n:(2*max_elt_sz) 

    let _empty_blk () = String.make buf_sz chr0 |> of_string

    let m_blk_id = blk_id_mshlr.mshl 
    let u_blk_id = blk_id_mshlr.umshl
    let m_elt = elt_mshlr.mshl 
    let u_elt = elt_mshlr.umshl



    (* These x_to_... functions marshal the whole lot at once,
       whereas typically we only marshal elts one at a time *)
    (* NOTE this returns the offset pointing to the None
       end-of-list marker *)
    let x_to_buf (elts,nxt) : buf*int = 
      (* write nxt *)
      create buf_sz |> fun buf ->
      (buf,0) |> m_blk_id nxt |> fun (buf,i) -> 
      assert(i<=max_blk_id_sz);
      (buf,off0,elts) |> iter_k (fun ~k (buf,off,elts) ->
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
      (off0,[]) |> iter_k (fun ~k (off,acc) -> 
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
  end

  let plist_marshal_ops = A.m_u_ops


  (** Incorporate block device *)
  module B(S2: sig 
      val monad_ops: t monad_ops
      val blk_dev_ops: (blk_id,blk,t)blk_dev_ops
      val sync: unit -> (unit,t)m
    end) = struct
    open S2

    (************************************
     * Now incorporate the block device *
     ************************************)

    open (struct
      let ( >>= ) = monad_ops.bind 
      let return = monad_ops.return 
      let { write; read; _ } = blk_dev_ops 

      let write_sync ~blk_id ~blk = 
        write ~blk_id ~blk >>= fun () ->
        sync ()

    end)
    
    let mk_empty blk_id = 
      A.x_to_buf ([],None) |> fun (buf,off) -> 
      let pl = { hd=blk_id;
                 tl=blk_id;
                 blk_len=1;
                 buffer=buf;
                 off;
                 (* nxt_is_none=true; *)
                 dirty=true;
               }
      in
      let blk = A.buf_to_blk buf in
      write_sync ~blk_id ~blk >>= fun () ->
      return {pl with dirty=false}


    let read_from_head blk_id = 
      (blk_id,[]) |> iter_k (fun ~k (blk_id,acc) -> 
          read ~blk_id >>= fun blk ->
          A.blk_to_x blk |> fun (elts,nxt) ->
          match nxt with 
          | None -> 
            let acc = (elts,nxt)::acc in
            return (List.rev acc)
          | Some nxt -> k (nxt,(elts,Some nxt)::acc))


    (* FIXME rename to eg create_plist_from_tl *)
    let init_from_endpts ~hd ~tl ~blk_len = 
      read ~blk_id:tl >>= fun blk ->
      A.blk_to_x blk |> fun (elts,nxt) ->
      (elts,nxt) |> A.x_to_buf |> fun (buf,off) ->                  
      return { hd; tl; buffer=buf; off; blk_len; dirty=false }

    (*****************************************************************
     * Initialization and constructing the operations via with-state *
     *****************************************************************)

    let init = 
      object
        method mk_empty = mk_empty
        method read_from_hd = read_from_head
        method from_endpts = fun pl_root_info -> 
          let Pl_origin.{hd;tl;blk_len} = pl_root_info in
          init_from_endpts ~hd ~tl ~blk_len >>= fun plist ->
          return plist

      end

    (** Incorporate state passing *)
    module C(S3: sig
        val with_state:((blk_id,buf)plist,t) with_state
      end) = struct
      open S3
      (* working with_state *)

      (***************************
       * State passing functions *
       ***************************)

      (* add element in tl; don't use blk_dev *)
      let add_elt ~state ~elt = 
        let buf,off = state.buffer,state.off in
        assert(A.can_fit_elt off);
        (* write an elt and update the pointer offset *)
        (buf,off) |> A.m_elt (Some elt) |> fun (buf,off) -> 
        {state with buffer=buf; off; dirty=true}

      (* don't use blk_dev *)
      let set_nxt nxt buf =
        (* update nxt first *)
        (buf,0) |> A.m_blk_id nxt |> fun (buf,_) -> 
        buf

      (* don't use blk_dev *)
      let move_to_nxt ~hd ~nxt ~blk_len ~buf ~off ~dirty =
        (* assert(buf_to_x state.buffer |> snd = nxt); *)
        { hd; tl=nxt; blk_len; buffer=buf; off; (* nxt_is_none=true; *) dirty }


      let add_elt_list_terminator off buf = 
        (buf,off) |> A.m_elt None |> fun (buf,_) -> 
        buf


      (*********************************************
       * Functions that don't use the block device *
       *********************************************)

      let blk_len () = 
        with_state.with_state (fun ~state ~set_state ->
            return state.blk_len)

      let add_if_room elt = 
        with_state.with_state (fun ~state ~set_state ->
            let { off; _ } = state in
            match A.can_fit_elt off with
            | true -> add_elt ~state ~elt |> fun s ->
                      set_state s >>= fun () ->
                      return true
            | false -> return false)

      let get_origin () = 
        with_state.with_state (fun ~state ~set_state ->
            return Pl_origin.{hd=state.hd;tl=state.tl;blk_len=state.blk_len})

      (**********************************
       * Functions that use the blk dev *
       **********************************)

                  (*
                  let _clear_nxt_blk blk_id = 
                    A.x_to_blk ([],None) |> fun blk ->
                    write ~blk_id ~blk
                     *)

      (* not used 
         (* invariant: if tl not written to disk yet, then tl_dirty is true *)
         (* NOTE up to this point, presumably as a very minor
         optimization we haven't written the eol
         terminator *)
         let sync' ~state = 
         let { tl; buffer=buf; dirty; off; _ } = state in
         match dirty with
         | true -> (
            (* marshal None at off *)
            add_elt_list_terminator off buf |> fun buf ->
            write ~blk_id:tl ~blk:(A.buf_to_blk buf) >>= fun () ->
            return {state with dirty=false })
         | false -> return state
      *)

      let add = 
        fun ~nxt ~elt ->
        with_state.with_state (fun ~state ~set_state ->
            let { off; _ } = state in
            match A.can_fit ~off ~n:(2*A.max_elt_sz) with
            | true -> (
                add_elt ~state ~elt |> fun state ->
                set_state state >>= fun () ->
                return (Some nxt))
            | false -> (
                (* write the new blk first *)
                A.x_to_buf ([elt],None) |> fun (buf,off) ->
                write_sync ~blk_id:nxt ~blk:(A.buf_to_blk buf) >>= fun () ->
                (* write old blk with nxt *)
                let blk_id = state.tl in
                let buf' = state.buffer |> set_nxt (Some nxt)
                           |> add_elt_list_terminator state.off
                in
                write_sync ~blk_id ~blk:(buf' |> A.buf_to_blk) >>= fun () ->
                move_to_nxt ~hd:state.hd ~nxt ~blk_len:(state.blk_len+1) ~buf ~off ~dirty:false 
                |> set_state >>= fun () ->
                return None))

      let sync_tl =
        fun () ->
        with_state.with_state (fun ~state ~set_state ->
            let { tl=blk_id; buffer=buf; off; dirty; _ } = state in
            buf |> add_elt_list_terminator off |> fun buf ->
            write_sync ~blk_id ~blk:(A.buf_to_blk buf) >>= fun () ->
            set_state {state with buffer=buf; dirty=false})

      let adv_hd () = 
        with_state.with_state (fun ~state ~set_state ->
            let { hd; _ } = (state:(_,_)plist) in
            read ~blk_id:hd >>= fun old_blk ->
            old_blk |> A.blk_to_x |> fun (elts,nxt) ->        
            match nxt with
            | None -> return (Error ())
            | Some new_hd -> 
              set_state { state with hd=new_hd;blk_len=state.blk_len-1 } >>= fun () ->
              return (Ok { old_hd=hd; old_elts=elts; new_hd }))

      let adv_tl nxt = 
        with_state.with_state (fun ~state ~set_state ->
            (* write the new blk first *)
            A.x_to_buf ([],None) |> fun (buf,off) ->
            write_sync ~blk_id:nxt ~blk:(A.buf_to_blk buf) >>= fun () ->
            (* write old blk with nxt *)
            let blk_id = state.tl in
            let buf' = state.buffer |> set_nxt (Some nxt)
                       |> add_elt_list_terminator state.off
            in
            write_sync ~blk_id ~blk:(buf' |> A.buf_to_blk) >>= fun () ->
            move_to_nxt ~hd:state.hd ~nxt ~blk_len:(state.blk_len+1) ~buf ~off ~dirty:false
            |> set_state >>= fun () ->
            return ())


      (* Read a blk, without assuming that it is a marshalled plist block *)
      let read_blk blk_id = 
        read ~blk_id >>= fun blk ->
        try 
          let x = A.blk_to_x blk in
          return (Ok(A.blk_to_x blk))
        with _ -> return (Error ())

      (* Read the hd blk (assuming hd <> tl) from disk *)
      let read_hd () =
        get_origin () >>= fun rinf -> 
        (* FIXME following should be a warning on log *)
        assert(
          (match rinf.hd=rinf.tl with 
           | true -> Printf.printf "Warning: read_hd, attempt to read \
                                    hd when hd=tl; hd may not be synced" 
           | false -> ()); 
          true);
        read_blk rinf.hd >>= fun r ->
        match r with
        | Ok x -> return x
        | Error () -> failwith "read_hd: hd block was not marshalled \
                                correctly on disk; are you sure it was \
                                synced?"

      (* FIXME perhaps move this outside the plist operations, to init/with_state level *)
      let append pl2 = 
        with_state.with_state (fun ~state ~set_state -> 
            let buf = state.buffer |> set_nxt (Some(pl2.hd)) in
            write_sync ~blk_id:state.tl ~blk:(A.buf_to_blk buf) >>= fun () ->
            let { hd; tl; buffer; off; blk_len; dirty=_ } = state in
            set_state { hd; tl=pl2.tl; buffer=pl2.buffer; off=pl2.off; 
                        blk_len=(blk_len+pl2.blk_len); dirty=pl2.dirty })
                      (*
                      let read_tl =
                        get_hd_tl () >>= fun (hd_,tl) ->
                        read_blk tl
                      in
                         *)

      let plist_ops = 
        { add;add_if_room;sync_tl;blk_len;adv_hd;adv_tl;get_origin;read_hd;append }

      let _ : (a, buf, blk_id, t) plist_ops = plist_ops
    end (* C *)
  end (* B *)

  let plist_factory ~monad_ops : (_,_,_,_,_)plist_factory = 
    object
      method monad_ops = monad_ops
      method buf_ops = buf_ops
      method blk_ops = blk_ops
      method plist_marshal_info = plist_marshal_info
      method plist_marshal_ops = plist_marshal_ops
      method with_blk_dev_ops =
        fun ~blk_dev_ops ~sync -> 
        let module S2 = struct 
          let monad_ops=monad_ops 
          let blk_dev_ops = blk_dev_ops 
          let sync = sync
        end
        in
        let module B = B(S2) in
        object
          method init = B.init
          method with_state = fun with_state -> 
            let module C = B.C(struct let with_state = with_state end) in
            C.plist_ops
              
          method with_ref = fun (pl:(_,_)plist) -> 
            let r = ref pl in
            let with_plist = Tjr_monad.with_imperative_ref ~monad_ops r in
            object
              method plist_ref=r
              method with_plist=with_plist
            end

          method add_origin=fun 
            (obj:<set_and_sync: 'blk_id Pl_origin.t ->(unit,'t)m>) plist_ops -> 
            let ( >>= ) = monad_ops.bind in
            let return = monad_ops.return in
            let set_and_sync = obj#set_and_sync in
            (* pick out those operations that can modify the origin,
               and alter so that they correctly sync the origin block
               *)
            let { add; adv_hd; adv_tl; append; get_origin; _ } = plist_ops in
            let sync_origin () = 
              get_origin () >>= fun o -> 
              set_and_sync o
            in
            let add ~nxt ~elt = 
              add ~nxt ~elt >>= function
              | None -> sync_origin() >>= fun () -> return None
              | Some _ as x-> return x
            in
            let adv_hd () = 
              adv_hd () >>= fun r -> 
              sync_origin () >>= fun () -> 
              return r
            in
            let adv_tl blk_id = 
              adv_tl blk_id >>= fun () -> 
              sync_origin () 
            in
            let append pl = 
              append pl >>= fun () -> 
              sync_origin () 
            in
            { plist_ops with add; adv_hd; append }              
        end
    end

  let _ : 
    monad_ops:t Tjr_monad.monad_ops ->
    (a, blk_id, blk, buf, t) plist_factory 
    = plist_factory

end (* A *)

(** Like Make_v1 but with restricted sig *)
module Make_v2(S:S) : T with module S=S = struct
  include Make_v1(S)
end


(** Version without functor *)
let make (type buf blk_id blk t a) ~buf_ops ~blk_ops ~plist_marshal_info ~monad_ops = 
  let module S = struct
    type nonrec buf=buf
    type nonrec blk_id=blk_id
    type nonrec blk=blk
    type nonrec t=t
    type nonrec a=a
    let buf_ops,blk_ops,plist_marshal_info=
      buf_ops,blk_ops,plist_marshal_info
  end
  in
  let module M = Make_v2(S) in
  M.plist_factory ~monad_ops
