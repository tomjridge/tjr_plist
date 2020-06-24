(** Like [Make_1], but outputs only a plist_factory; this is the preferred intf *)

open Plist_intf

module type S = Make_1.S

module type T = sig
  include S
      
  val plist_factory : 
    monad_ops:t monad_ops -> 
    buf_ops:buf buf_ops ->
    blk_ops:blk blk_ops ->
    plist_marshal_info:('a, blk_id, blk, buf) plist_marshal_info ->
    ('a, blk_id, blk, buf, t) plist_factory
end

module Make(S:S) : T with 
  type buf=S.buf and type blk_id=S.blk_id 
                 and type blk=S.blk and type t = S.t 
= struct
  include S
  include Make_1.Make(S)
end
