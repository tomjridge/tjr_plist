(** A freelist providing alloc and free, based on plist; safe to open this module. 

{%html:
<img src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">

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


(** {2 Example and summary} *)

module Freelist_example = Freelist_example


module Summary = Summary
