(** A freelist providing alloc and free, based on plist;
   concurrent-safe ((multiple writers are allowed, but only a single
   thread interacts with disk); safe to open this module.

{%html: <img width='100%'
   src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">

%}

*)

include Summary


module Freelist_intf = Freelist_intf

(** {2 Make functors} *)

module Fl_make_1 = Fl_make_1


(** {2 Examples and summary} *)

let fl_examples = 
  let open Freelist_intf in
  let open Shared_ctxt in
  let open Plist_intf in
  let module M = struct
    (* open Fl_origin *)
    type t = (r,r) Fl_origin.t[@@deriving bin_io]
    let max_sz = 9*3 + 10 
    (* assume hd,tl,blk_len and min_free option can be marshalled in this many bytes *)
  end
  in  
  let origin_mshlr = 
    bp_mshlrs#ba_mshlr 
      ~mshlr:(module M) 
      ~buf_sz:(Blk_sz.to_int blk_sz_4096) 
  in  
  let module Origin_mshlr = (val origin_mshlr) in

  (* util; we read the fl_origin then need to instantiate the plist *)
  let origin_to_pl Fl_origin.{hd;tl;blk_len;_} = Pl_origin.{hd;tl;blk_len} in
  
  (* for Shared_ctxt.r *)
  let fact : (r,_,_,_) freelist_factory = 
    let version = { e2b=(fun x -> x); b2e=(fun x->x) } in
    let origin_ops = 
      fun ~(blk_dev_ops:(_,_,_)blk_dev_ops) ~barrier ~sync ~blk_id -> 
        Fl_origin.{
          read=(fun () ->  
              blk_dev_ops.read ~blk_id >>= fun blk ->
              Origin_mshlr.unmarshal blk |> return);
          write=(fun t -> 
              Origin_mshlr.marshal t |> fun blk -> 
              blk_dev_ops.write ~blk_id ~blk);
          (* barrier=barrier; *)
          (* sync=sync *)
        }
    in
    let with_ = (
      fun  
        ~(blk_dev_ops:(_,_,_)blk_dev_ops) 
        ~barrier
        ~sync
        ~(origin_ops:(_,_,_)Fl_origin.ops) 
        ~params 
        ->
          object
            method plist_ops pl_origin = 
              pl_origin |> fun Pl_origin.{hd;tl;blk_len} -> 
              let open (struct
                let fact = pl_examples#for_blk_id

                let _ : r Pl_type_abbrevs.plist_factory = fact

                let x = fact#with_blk_dev_ops ~blk_dev_ops ~barrier:barrier

                let plist_ops = 
                  x#init#from_endpts Plist_intf.Pl_origin.{hd; tl; blk_len} >>= fun pl -> 
                  x#with_ref pl |> fun y -> 
                  return (x#with_state y#with_plist)
              end)
              in
              plist_ops

            method with_plist_ops (plist_ops:(_,_,_,_)plist_ops) = (
              let with_state = fun with_state -> 
                Fl_make_1.make (object
                  method monad_ops=monad_ops
                  method async=async
                  method event_ops=event_ops
                  method barrier=barrier
                  method sync=sync
                  method origin_ops=origin_ops
                  method params=params
                  method plist_ops=plist_ops
                  method version=For_blkids version
                  method with_freelist=with_state
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
              end)
          end)
    in
    object
      method version=version
      method empty_freelist=empty_freelist
      method origin_ops=origin_ops
      method with_=with_
    end
  in
  object
    method origin_to_pl : 'a 'b. ('a,'b)Fl_origin.t -> 'b Pl_origin.t = origin_to_pl 
    method freelist_factory=fact
    method empty_freelist ~min_free:ropt = 
      let min_free = 
        match ropt with 
        | None -> None
        | Some min_free -> 
          Some(min_free,
               { min_free_alloc=
                   fun (r:r) n -> 
                     let start = B.to_int r in
                     let end_ = start+n in
                     (List_.map_range ~f:B.of_int start end_,B.of_int end_)})
      in
      empty_freelist ~min_free
  end

let _ = fl_examples

(* module Freelist_example = Freelist_example *)


module Summary = Summary
