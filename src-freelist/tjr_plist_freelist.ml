(** A freelist, based on plist; don't open this module. *)

module Freelist_intf = Freelist_intf

module Freelist_make = Freelist_make

module Make_2 = Make_2

module Make_3 = Make_3


(** {2 Summary} *)

open Tjr_plist.Plist_intf
(* open Freelist_intf *)
open Freelist_make

include Freelist_intf

let make : 
< async : (unit -> (unit, 't) m) -> (unit, 't) m;
  event_ops : 't Tjr_monad.Event.event_ops;
  monad_ops : 't monad_ops;
  plist : ('elt, 'buf, 'blk_id, 't) plist_ops;
  root_block : ('blk_id, 't) root_block_ops;
  version : ('elt, 'blk_id, 't) version;
  with_freelist : ('elt freelist, 't) with_state;
> -> ('elt, 't) freelist_ops
= make
(** 

{[
let make : 
< async : (unit -> (unit, 't) m) -> (unit, 't) m;
  event_ops : 't Tjr_monad.Event.event_ops;
  monad_ops : 't monad_ops;
  plist : ('elt, 'buf, 'blk_id, 't) plist_ops;
  root_block : ('blk_id, 't) root_block_ops;
  version : ('elt, 'blk_id, 't) version;
  with_freelist : ('elt freelist, 't) with_state;
> -> ('elt, 't) freelist_ops
= make
]}

*)
