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
      ~root_mshlr 
      ~blk_id 
      (* : ('a,'blk,'t) root_blk_ops *)
    =
    let { mshl; umshl; max_elt_sz } = root_mshlr in
    assert(max_elt_sz <= (blk_dev_ops.blk_sz |> Blk_sz.to_int));
    let root_write r = 
      let blk = blk_ops.of_string (String.make 4096 'x') in
      mshl r (blk,0) |> fun (blk,_) ->
      blk_dev_ops.write ~blk_id ~blk
    in
    let root_read () = 
      blk_dev_ops.read ~blk_id >>= fun blk -> 
      umshl blk 0 |> fun (r,_) -> 
      return r
    in
    {root_write;root_read}
    
  let _ = make_root_blk_ops
end
open Root_blk

open Bin_prot.Std





module Internal = struct
  module B = Blk_id_as_int
  let [b0;b1;b2] = List.map B.of_int [0;1;2][@@warning "-8"]
end
open Internal



(* FIXME move these later *)
let blk_ops = Blk_factory.(make_3 ())

let blk_dev' = Blk_dev_factory.(make_5)

type root_blk = {
  hd:B.blk_id;
  tl:B.blk_id;
  blk_len:int;
  min_free_blk_id:int
}[@@deriving yojson, bin_io]





module Internal_rb = struct

  module Rb = Marshal_factory.Make_marshaller(
    struct type t = root_blk[@@deriving bin_io] let max_elt_sz = 40 end)

  let rb_mshlr = Rb.mshlr

  let rb_ops blk_dev_ops = 
    Root_blk.make_root_blk_ops 
      ~blk_ops
      ~blk_dev_ops
      ~root_mshlr:rb_mshlr
      ~blk_id:b0

  let _ = rb_ops  (* FIXME change this to a pl marshaller *)

end
open Internal_rb




let filename = "plist_on_disk.store"



module L = Tjr_monad.With_lwt
open L

(* NOTE guard creation of r6 till post-runtime-init *)
module Make() = struct

  let r6 : ((module Blk_dev_factory.R6), lwt) m = Blk_dev_factory.(make_8 filename)

  let _ = r6

  module With_blk_dev(X:Blk_dev_factory.R6) = struct
    open X

    module F = Plist_factory 

    let F.{plist_marshal_ops;plist_extra_ops;plist_ops} = 
      F.make_2 blk_dev_ops

    let rb_ops = rb_ops blk_dev_ops

    let _ = rb_ops


    (* FIXME didn't we standardize these operations somewhere? yes, as
       extra_ops :( *)
    (* FIXMe note that this doesn't write an initial root blk *)
    let create_plist () = plist_extra_ops.create_plist b1 

    (* FIXME separate into rb2pl and read *)
    let read_root_blk () = 
      rb_ops.root_read () >>= fun x -> 
      x |> fun {hd;tl;blk_len;min_free_blk_id} ->
      (* FIXME need a read_blk in extra ops *)
      plist_extra_ops.read_plist_tl ~hd ~tl ~blk_len >>= fun pl ->     
      return (pl,min_free_blk_id)

    let _ = read_root_blk

    module With_pl(Y:sig 
        val pl: (B.blk_id, ba_buf) plist 
        val min_free_blk_id: int
      end) 
      = (
      struct    

        let min = ref Y.min_free_blk_id

        let pl_to_rb (pl:('a,'b)plist) : root_blk = 
          { hd=pl.hd; tl=pl.tl; blk_len=pl.blk_len; min_free_blk_id= !min }

        let pl_ref : (B.blk_id, ba_buf) plist ref = ref Y.pl

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
        val plist_ops : (int, ba_buf, B.blk_id, lwt) plist_ops
        val close_plist_and_blk_dev : unit -> (unit, lwt) m
      end)

  end
end


module Test() = struct
  open Tjr_monad.With_lwt

  let test = 
    Printf.printf "plist_on_disk: test starting...\n";
    let ten_k = 10_000 in
    
    (* create and write some elts *)
    let module M = Make() in
    M.r6 >>= fun r6 ->
    let module R6 = (val r6) in
    (* let open R6 in *)
    let module X = M.With_blk_dev(R6) in
    X.create_plist () >>= fun pl -> 
    (* FIXME rb should come before we establish the pl *)
    X.rb_ops.root_write
      { hd=pl.hd; tl=pl.tl; blk_len=pl.blk_len; min_free_blk_id=2 }
    >>= fun () ->
    let module Y = 
      X.With_pl (struct let pl = pl let min_free_blk_id = 2 end) in
    Y.add (List_.from_to 1 (ten_k+1)) >>= fun () ->
    Y.close_plist_and_blk_dev () >>= fun () -> 
    
    (* now read back *)
    let module M = Make() in
    M.r6 >>= fun r6 ->
    let module R6 = (val r6) in
    let module X = M.With_blk_dev(R6) in
    X.read_root_blk () >>= fun (pl,n) ->
    Printf.printf "Read root blk, min is %#d\n" n;
    X.plist_extra_ops.read_plist pl.hd >>= fun xs ->
    xs |> List.map (fun (ys,nx) -> ys) |> List.concat |> fun xs ->
    Printf.printf "Read back: %#d elts\n%!" (List.length xs);
    assert (List.length xs = ten_k);
    let module Y = 
      X.With_pl (struct let pl = pl let min_free_blk_id = n end) in
    (* let open Y in *)
    Y.add (List_.from_to 20_001 30_000) >>= fun () ->
    Y.close_plist_and_blk_dev () >>= fun () -> 
    Printf.printf "plist_on_disk: test finished\n%!";
    return ()
end
