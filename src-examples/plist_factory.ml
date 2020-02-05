(** Common instances of plist *)

module M1 = struct
  type blk_id = Blk_id_as_int.blk_id 
  type buf = ba_buf
  type blk = ba_buf
end

type 'a arg = 
  | A1 of { 
      max_elt_sz:int;
      m_elt : 'a option -> (M1.buf * int) -> M1.buf * int;
      u_elt : M1.buf -> int -> 'a option * int;
      blk_dev_ops:(M1.blk_id,M1.blk,lwt)blk_dev_ops
 }
(** 
- A1: elt is arb, with bigarray and lwt 
*)


(* FIXME following is a WIP; staging as usual *)

type ('a,'t) res = 
  | R1 of ('a,'t) r1

and ('a,'t) r1 = { 
  plist_marshal_ops: ('a,M1.blk_id,M1.buf) plist_marshal_ops;
  plist_extra_ops: ('a,M1.buf,M1.blk_id,'t) plist_extra_ops;
  plist_ops: 
    (('a, M1.blk_id, M1.buf) plist, lwt) with_state ->
    ('a, M1.buf, M1.blk_id, M1.buf, 't) plist_ops;
}

