(** Summary, for documentation purposes *)

open Tjr_plist.Plist_intf
(* open Freelist_intf *)
open Fl_make_1

include Freelist_intf

let make : 
< async : (unit -> (unit, 't) m) -> (unit, 't) m;
  event_ops : 't event_ops;
  monad_ops : 't monad_ops;
  plist : ('elt, 'buf, 'blk_id, 't) plist_ops;
  root_block : ('elt,'blk_id, 't) fl_root_ops;
  version : ('elt, 'blk_id, 't) version;
  with_freelist : ('elt freelist, 't) with_state;
> -> ('elt, 't) freelist_ops
= make
(** 

{[
let make : 
< async : (unit -> (unit, 't) m) -> (unit, 't) m;
  event_ops : 't event_ops;
  monad_ops : 't monad_ops;
  plist : ('elt, 'buf, 'blk_id, 't) plist_ops;
  root_block : ('blk_id, 't) root_block_ops;
  version : ('elt, 'blk_id, 't) version;
  with_freelist : ('elt freelist, 't) with_state;
> -> ('elt, 't) freelist_ops
= make
]}

*)
