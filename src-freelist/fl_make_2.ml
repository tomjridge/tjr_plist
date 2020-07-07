(** A layer over fl_make_1, to target the factory interface.

FIXME fl_make_1 should target the interface without the updates to
   origin blk; then this layer can add that in

*)


open Plist_intf
open Shared_ctxt
open Freelist_intf

(** Example for blk_ids *)
module Fl_example_1 = struct
  let version = { e2b=(fun x -> x); b2e=(fun x->x) }

  let empty_freelist ~min_free = Freelist_intf.empty_freelist ~min_free

  module M = struct
    (* open Fl_origin *)
    type t = (r,r) Fl_origin.t[@@deriving bin_io]
    let max_sz = 9*3 + 10 
    (* assume hd,tl,blk_len and min_free option can be marshalled in this many bytes *)
  end

  let origin_mshlr = 
    bp_mshlrs#ba_mshlr 
      ~mshlr:(module M) 
      ~buf_sz:(Blk_sz.to_int blk_sz_4096) 

  module Origin_mshlr = (val origin_mshlr) 

  let read_origin ~blk_dev_ops ~blk_id =
    blk_dev_ops.read ~blk_id >>= fun blk ->       
    Origin_mshlr.unmarshal blk |> return

  let write_origin ~blk_dev_ops ~blk_id ~origin =
    Origin_mshlr.marshal origin |> fun blk ->
    blk_dev_ops.write ~blk_id ~blk


  (* util; we read the fl_origin then need to instantiate the plist *)
  let fl_origin_to_pl Fl_origin.{hd;tl;blk_len;_} = Pl_origin.{hd;tl;blk_len}

  module With_(S:sig
      val blk_dev_ops : (r,blk,t)blk_dev_ops
      val barrier : unit -> (unit,t)m
      val sync : unit -> (unit,t)m
      val params : params
    end) = struct
    open S

    let plist_ops pl_origin = 
      pl_origin |> fun Pl_origin.{hd;tl;blk_len} -> 
      let open (struct
        let fact = pl_examples#for_blk_id

        let _ : r Pl_type_abbrevs.plist_factory = fact

        let x = fact#with_blk_dev_ops ~blk_dev_ops ~barrier

        let plist_ops = 
          x#init#from_endpts Plist_intf.Pl_origin.{hd; tl; blk_len} >>= fun pl -> 
          x#with_ref pl |> fun y -> 
          return (x#with_state y#with_plist)
      end)
      in
      plist_ops

    let min_free_alloc =
      fun (r:r) n -> 
      let start = B.to_int r in
      let end_ = start+n in
      (List_.map_range ~f:B.of_int start end_,B.of_int end_)



    let with_plist_ops (plist_ops:(_,_,_,_)plist_ops) = 
      let with_state = fun with_state -> 
        Fl_make_1.make (object
          method monad_ops=monad_ops
          method event_ops=event_ops
          method async=async
          method sync=sync
          method plist_ops=plist_ops
          method with_freelist=with_state
          method version=For_blkids version
          method params=params
          method min_free_alloc=min_free_alloc
        end)
      in
      let with_locked_ref = fun fl ->
        let freelist_ref = ref fl in
        let with_state' = With_lwt.with_locked_ref freelist_ref in
        let freelist_ops = with_state with_state' in
        object
          method freelist_ops=freelist_ops
          method freelist_ref=freelist_ref
        end
      in
      object
        method with_state=with_state
        method with_locked_ref=with_locked_ref
      end


    (* NOTE this doesn't currently sync the origin FIXME *)
    let from_origin blk_id = 
      read_origin ~blk_dev_ops ~blk_id >>= fun origin ->
      let fl = empty_freelist ~min_free:origin.min_free in
      let pl_origin = fl_origin_to_pl origin in
      plist_ops pl_origin >>= fun plist_ops ->
      with_plist_ops plist_ops |> fun x -> 
      x#with_locked_ref fl |> return
      

  end (* With_ *)

  let with_ ~blk_dev_ops ~barrier ~sync ~params =
    let module S = struct
      let blk_dev_ops=blk_dev_ops
      let barrier=barrier
      let sync=sync
      let params=params
    end
    in
    let module X = With_(S) in      
    object
      method plist_ops=X.plist_ops
      method with_plist_ops=X.with_plist_ops
      method from_origin=X.from_origin
    end

  let factory : _ freelist_factory = object
    method version=version
    method empty_freelist=empty_freelist
    method read_origin=read_origin
    method write_origin=write_origin
    method fl_origin_to_pl=fl_origin_to_pl
    method with_=with_
  end

end


(* FIXME also add an example for ints *)
let fl_examples = object
  method for_r=Fl_example_1.factory
  method for_int=failwith "FIXME"
end  
