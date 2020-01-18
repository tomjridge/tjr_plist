(** A persistent (on-disk) list. 

Best to open just this module (not intf).
*)

module Plist_intf = Plist_intf

include Plist_intf.Plist_marshal_info

module Make_ = Make_

let make : monad_ops:'t monad_ops ->
buf_ops:'buf buf_ops ->
blk_ops:'blk blk_ops ->
blk_dev_ops:('blk_id, 'blk, 't) blk_dev_ops ->
marshal_info:('a, 'blk_id, 'blk, 'buf) plist_marshal_info ->
('a, 'blk_id, 'blk, 'buf, 't) Make_.ret_ = Make_.make
