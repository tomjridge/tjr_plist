(** A freelist providing alloc and free, based on plist;
   concurrent-safe ((multiple writers are allowed, but only a single
   thread interacts with disk); safe to open this module.

{%html: <img
   src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">

%}

*)

include Summary


module Freelist_intf = Freelist_intf

(** {2 Make functors} *)

module Fl_make_1 = Fl_make_1

(* module Fl_make_2 = Fl_make_2 *)

(* module Fl_make_3 = Fl_make_3 *)

(*
let fl_examples =
  let open Freelist_intf in
  let open Sh_std_ctxt in
  let open Tjr_monad.With_lwt in
  let int_plist_factory = pl_examples#int_plist_factory in
  object
    method int_freelist_ops = 
      fun (x1:< 
           blk_dev_ops : (blk_id,blk,t)blk_dev_ops;  
           pl_params   : < hd:blk_id; tl:blk_id; blk_len:int > ;
           fl_root_blk : blk_id; >)
        -> 
          let blk_dev_ops = x1#blk_dev_ops in
          int_plist_factory#with_blk_dev_ops ~blk_dev_ops |> fun x2 -> 
          x2#from_disk x1#pl_params >>= fun x3 -> 
          let plist_ops = x3#plist_ops in          
          let fl_fact = 
          return (object
            method freelist_ops: (int,t) freelist_ops = failwith ""
          end)

  end
*)  


(** {2 Examples and summary} *)

let fl_examples = 
  let open Freelist_intf in
  let open Shared_ctxt in
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
  let fact : (_,_,_,_) freelist_factory = 
    let version = { e2b=(fun x -> x); b2e=(fun x->x) } in
    let origin_ops = 
      fun ~(blk_dev_ops:(_,_,_)blk_dev_ops) ~origin_blkid ~sync_origin -> 
        Fl_origin.{
          read=(fun () ->  
              blk_dev_ops.read ~blk_id:origin_blkid >>= fun blk ->
              Origin_mshlr.unmarshal blk |> return);
          write=(fun t -> 
              Origin_mshlr.marshal t |> fun blk -> 
              blk_dev_ops.write ~blk_id:origin_blkid ~blk);
          sync=(fun () ->
              Printf.printf "FIXME freelist origin sync called in %s\n%!" __FILE__;
              return ())
        }
    in
    let with_ = 
      fun  ~(blk_dev_ops:(_,_,_)blk_dev_ops) ~sync_blk_dev ~origin_ops ~params -> 
        let with_state = fun with_state -> 
          Fl_make_1.make (object
            method monad_ops=monad_ops
            method async=async
            method event_ops=event_ops
            method origin_ops=origin_ops
            method params=params
            method plist_ops=failwith ""
            method version=For_blkids version
            method with_freelist=with_state
          end)
        in
        let with_ref = fun fl ->
          let freelist_ref = ref fl in
          let with_state' = Tjr_monad.with_imperative_ref ~monad_ops freelist_ref in
          let freelist_ops = with_state with_state' in
          object
            method freelist_ops=freelist_ops
            method freelist_ref=freelist_ref
          end
        in
        object
          method with_state=with_state
          method with_ref=with_ref
        end
    in
    object
      method version=version
      method origin_ops=origin_ops
      method with_=with_
    end
  in
  object
    method freelist_factory=fact
  end
  


module Freelist_example = Freelist_example


module Summary = Summary
