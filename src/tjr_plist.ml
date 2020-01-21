(** A persistent (on-disk) list. 

Best to locally open just this module (which includes intf).
*)

module Plist_intf = Plist_intf
include Plist_intf

let make : plist_marshal_info:('a, 'blk_id, 'blk, 'buf) plist_marshal_info ->
buf_ops:'buf buf_ops ->
blk_ops:'blk blk_ops ->
('a, 'blk_id, 'blk) plist_marshal_ops *
[ `K1 of
     monad_ops:'b monad_ops ->
     blk_dev_ops:('blk_id, 'blk, 'b) blk_dev_ops ->
     ('a, 'buf, 'blk_id, 'b) plist_extra_ops *
     [ `K2 of
          with_state:(('c, 'blk_id, 'buf) plist, 'b) with_state ->
          ('a, 'd, 'blk_id, 'e, 'b) plist_ops ] ] = Make_.make
