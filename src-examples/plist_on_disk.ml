(** A persistent list, stored on disk; blk 0 stores the hd,tl and
   min_free_blk_id

| blk idx | stores                    |
|---------+---------------------------|
|       0 | hd,tl,min_free_blk_id     |
|       1 | start of plist (initially)|
|         |                           |

 *)
open Marshal_factory

(* generic root block reader/writer FIXME move this *)
module Root_blk = struct

  open Tjr_monad.With_lwt

  (* FIXME make this take a blk_id so we don't have to track these operations per blk_id *)
  type ('a,'t) root_blk_ops = {
    root_write: 'a -> (unit,'t)m;
    root_read: unit -> ('a,'t)m;
  }

  (* FIXME if we have blk_dev_ops we surely have blk_ops already *)
  let make_root_blk_ops
      ~blk_ops 
      ~(blk_dev_ops:('blk_id,'blk,'t)blk_dev_ops)
      ~marshaller 
      ~blk_id 
      (* : ('a,'blk,'t) root_blk_ops *)
    =
    assert(marshaller.max_elt_sz <= (blk_dev_ops.blk_sz |> Blk_sz.to_int));
    let root_write r = 
      let blk = blk_ops.of_string "" in
      marshaller.m_elt r (blk,0) |> fun (blk,_) ->
      blk_dev_ops.write ~blk_id ~blk
    in
    let root_read () = 
      blk_dev_ops.read ~blk_id >>= fun blk -> 
      marshaller.u_elt blk 0 |> fun (r,_) -> 
      return r
    in
    {root_write;root_read}
    
  let _ = make_root_blk_ops
end
open Root_blk

open Bin_prot.Std





module Internal = struct
  module B = Blk_id_as_int
end
open Internal

let blk_ops = Blk_factory.(make A3_ba_4096 |> fun (R3 x) -> x)[@@warning "-8"]

let blk_dev' = Blk_dev_factory.(
    make A5_blk_ba__lwt_fd |> fun (R5 f) -> f)[@@warning "-8"]

type root_blk = {
  hd:B.blk_id;
  tl:B.blk_id;
  blk_len:int;
  min_free_blk_id:int
}[@@deriving yojson, bin_io]

let [b0;b1;b2] = List.map B.of_int [0;1;2][@@warning "-8"]

module Internal_rb = struct

  module Rb = Marshal_factory.Make_marshaller(
    struct type t = root_blk[@@deriving bin_io] let max_elt_sz = 40 end)

  let rb_marshaller = Rb.marshaller

  let rb_ops blk_dev_ops = 
    Root_blk.make_root_blk_ops 
      ~blk_ops
      ~blk_dev_ops
      ~marshaller:rb_marshaller
      ~blk_id:b0

  let _ = rb_ops  (* FIXME change this to a pl marshaller *)

end
open Internal_rb




let filename = "plist_on_disk.store"



module L = Tjr_monad.With_lwt
open L

let r6 : ((module Blk_dev_factory.R6), lwt) m = Blk_dev_factory.(make_6 (Filename filename))

let _ = r6

module With_blk_dev(X:Blk_dev_factory.R6) = struct
  open X

  module F = Plist_factory 

  let plist_marshal_ops,plist_extra_ops,plist_ops = 
    F.(make (A2_elt_int__lwt blk_dev) |> fun (R2 x) -> x)
    |> fun F.{plist_marshal_ops;plist_extra_ops;plist_ops} ->
    plist_marshal_ops,plist_extra_ops,plist_ops
  [@@warning "-8"]

  let rb_ops = rb_ops blk_dev
                 
  let _ = rb_ops


  let create () = 
    (* FIXME didn't we standardize these operations somewhere? *)
    plist_extra_ops.create_plist b1 >>= fun pl ->
    return pl

  (* FIXME separate into rb2pl and read *)
  let read_root_blk () = 
    rb_ops.root_read () >>= fun x -> 
    x |> fun {hd;tl;blk_len;min_free_blk_id} ->
    (* FIXME need a read_blk in extra ops *)
    plist_extra_ops.read_plist_tl ~hd ~tl ~blk_len >>= fun pl ->     
    return (pl,min_free_blk_id)

  let _ = read_root_blk

  module With_pl(Y:sig 
      val pl: (int, B.blk_id, ba_buf) plist 
      val min_free_blk_id: int
    end) 
    = (
    struct    

      let min = ref Y.min_free_blk_id

      let pl_to_rb (pl:('c,'a,'b)plist) : root_blk = 
        { hd=pl.hd; tl=pl.tl; blk_len=pl.blk_len; min_free_blk_id= !min }

      let pl_ref : (int, B.blk_id, ba_buf) plist ref = ref Y.pl

      let with_pl_ref =
        let with_state f = f ~state:(!pl_ref) ~set_state:(fun s -> pl_ref:=s; return ()) in
        { with_state }

      let write_root_blk () = rb_ops.root_write (pl_to_rb !pl_ref)

      let plist_ops = plist_ops with_pl_ref

      (* NOTE this doesn't write to disk *)
      let incr_min () = min:=!min + 1

      (* FIXME maybe add ~lo ~hi *)
      let add elts = 
        elts |> iter_k (fun ~k elts -> match elts with
            | [] -> return ()
            | elt::elts -> 
              plist_ops.add ~nxt:(B.of_int !min) ~elt >>= fun x -> 
              (match x with 
               | None -> incr_min ()
               | Some _ -> ());
              k elts)

      (* let read_plist () = (!pl_ref).hd |> plist_extra_ops.read_plist *)

      let close_plist_and_blk_dev () = 
        (* FIXME add close to plist_ops, which just calls sync_tl *)
        plist_ops.sync_tl () >>= fun () ->
        write_root_blk () >>= fun () ->
        close_blk_dev ()

    end : sig
      val add : int list -> (unit, lwt) m
      val plist_ops : (int, ba_buf, B.blk_id, ba_buf, lwt) plist_ops
      val close_plist_and_blk_dev : unit -> (unit, lwt) m
    end)

end
