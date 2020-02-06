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

  type ('a,'blk,'t) root_blk_ops = {
    root_write: 'a -> (unit,'t)m;
    root_read: unit -> ('a,'t)m;
  }

  (* FIXME if we have blk_dev_ops we surely have blk_ops already *)
  let make_root_blk_ops
      ~blk_ops 
      ~(blk_dev_ops:('blk_id,'blk,'t)blk_dev_ops)
      ~marshaller 
      ~blk_id 
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

module Internal_rb = struct

  module Rb = Marshal_factory.Make_marshaller(
    struct type t = root_blk[@@deriving bin_io] let max_elt_sz = 30 end)

  let rb_marshaller = Rb.marshaller

  let rb_ops blk_dev_ops = 
    Root_blk.make_root_blk_ops 
      ~blk_ops
      ~blk_dev_ops
      ~marshaller:rb_marshaller
      ~blk_id:(B.of_int 0)

  let init_root_blk = { hd=(B.of_int 1);tl=(B.of_int 1);blk_len=1;min_free_blk_id=2 }

end
open Internal_rb

(*
let init_blks = 
  (0,init_root_blk),
  (1,"empty_plist")

let first_plist_block = 1
*)

let filename = "plist_on_disk.store"

module L = Tjr_monad.With_lwt
open L

module F = Plist_factory 

module Make_fd() = struct
  let fd = 
    L.from_lwt (Lwt_unix.(openfile filename [O_CREAT;O_RDWR] Tjr_file.default_create_perm))
end

module With_fd(X:sig val fd:Lwt_unix.file_descr end) = struct
  open X

  let blk_dev = blk_dev' fd

  let plist_marshal_ops,plist_extra_ops,plist_ops = 
    F.(make (A2_elt_int__lwt blk_dev) |> fun (R2 x) -> x)
    |> fun F.{plist_marshal_ops;plist_extra_ops;plist_ops} ->
    plist_marshal_ops,plist_extra_ops,plist_ops
  [@@warning "-8"]

  let create () = 
    (* FIXME didn't we standardize these operations somewhere? *)
    plist_extra_ops.create_plist (B.of_int 1) >>= fun pl ->
    return pl

  let root_blk = ref init_root_blk

  let with_root_blk =
    let with_state f = f ~state:!root_blk ~set_state:(fun s -> root_blk:=s; return ()) in
    { with_state }

  (* at this point, we don't necessarily know what the initial state of the pl is *)


  let rb_ops = rb_ops blk_dev

  let pl_ref : (int, B.blk_id, ba_buf) plist option ref = ref None
      
  (* NOTE this assumes the pl_ref is Some by the time it is used *)
  let with_pl_ref =
    let with_state f = f ~state:(!pl_ref |> dest_Some) ~set_state:(fun s -> pl_ref:=Some s; return ()) in
    { with_state }

  (* as a side effect, this sets the root blk and also sets the pl ref *)
  let read_root_blk () = 
    rb_ops.root_read () >>= fun x -> 
    x |> fun {hd;tl;blk_len;min_free_blk_id} ->
    root_blk := x;
    (* FIXME need a read_blk in extra ops *)
    plist_extra_ops.read_plist_tl ~hd ~tl ~blk_len >>= fun pl -> 
    pl_ref := Some pl;
    return pl


  let write_root_blk () = 
    rb_ops.root_write (!root_blk)

  (* FIXME name the argument with_plist to avoid confusion with all
     the other with_ args *)
  let plist_ops = plist_ops with_pl_ref

  (* NOTE this doesn't write to disk *)
  let incr_min_free_blk_id () = 
    root_blk:={ (!root_blk) with min_free_blk_id=1+(!root_blk).min_free_blk_id }

  let add elts = 
    elts |> iter_k (fun ~k elts -> match elts with
        | [] -> return ()
        | elt::elts -> 
          plist_ops.add ~nxt:(B.of_int (!root_blk.min_free_blk_id)) ~elt >>= fun x -> 
          (match x with 
           | None -> incr_min_free_blk_id ()
           | Some _ -> ());
          k elts)

  let read_plist () = 
    (!root_blk).hd |> plist_extra_ops.read_plist

  let close () = 
    plist_ops.sync_tl () >>= fun () ->
    write_root_blk () >>= fun () ->
    L.from_lwt(Lwt_unix.close fd)

end
