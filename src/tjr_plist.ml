(** A persistent (on-disk) list. 

Look at {!Plist_intf} for the main types. {!Make_4} has the most succinct "make" function.
*)

module Plist_intf = Plist_intf
(* open Plist_intf *)
(* include Plist_intf *)

module Plist_sync_root_blk = Plist_sync_root_blk

module Make_1 = Make_1

module Make_3 = Make_3

module Make_4 = Make_4

module Plist_factory = Plist_factory


(*
let make : plist_marshal_info:('a, 'blk_id, 'blk, 'buf) plist_marshal_info ->
buf_ops:'buf buf_ops ->
blk_ops:'blk blk_ops ->
('a, 'blk_id, 'blk) plist_marshal_ops *
[ `K1 of
     monad_ops:'t monad_ops ->
     blk_dev_ops:('blk_id, 'blk, 't) blk_dev_ops ->
     ('a, 'buf, 'blk_id, 't) plist_extra_ops *
     [ `K2 of
          with_state:(('blk_id, 'buf) plist, 't) with_state ->
          (_, _, _, _) plist_ops ] ] = Make_1.make
*)

