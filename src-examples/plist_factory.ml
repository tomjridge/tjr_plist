(** Common instances of plist *)

open Marshal_factory

(** Internal:
- blk_id is Blk_id_as_int
- buf and blk are bigarray_4096 FIXME add to types
*)

module Internal = struct
  module M1 = struct
    type blk_id = Blk_id_as_int.blk_id 
    type buf = ba_buf
    type blk = ba_buf
  end
end

open Internal

type 'a arg =
  | A1_elt_arb__lwt of ('a option,M1.buf) marshaller * (M1.blk_id,M1.blk,lwt)blk_dev_ops
  | A2_elt_int__lwt of (M1.blk_id,M1.blk,lwt)blk_dev_ops (** *)
(** 
- A1: elt is arb, with lwt 
- A2: as A1, but elt is int
*)

(* FIXME following is a WIP; staging as usual *)

type 'a res = 
  | R1 of ('a,lwt) r1
  | R2 of (int,lwt) r1

and ('a,'t) r1 = { 
  plist_marshal_ops: ('a,M1.blk_id,M1.buf) plist_marshal_ops;
  plist_extra_ops: ('a,M1.buf,M1.blk_id,'t) plist_extra_ops;
  plist_ops: 
    (('a, M1.blk_id, M1.buf) plist, lwt) with_state ->
    ('a, M1.buf, M1.blk_id, M1.buf, 't) plist_ops;
}

(**/**)
let make_ (type a) ({ max_elt_sz; m_elt; u_elt }, blk_dev_ops) = 
  let open Plist_marshal_factory in
  let R3 { plist_marshal_info; buf_ops; blk_ops } = 
    (A3_elt_arbitrary__blk_ba_buf__buf_ba_buf { max_elt_sz; m_elt; u_elt })
    |> make
  [@@warning "-8"] 
  in
  Tjr_plist.make ~plist_marshal_info ~buf_ops ~blk_ops |> fun (plist_marshal_ops,`K1 rest) -> 
  rest ~monad_ops:lwt_monad_ops ~blk_dev_ops |> fun (plist_extra_ops,`K2 rest) -> 
  let plist_ops with_state = rest ~with_state in
  ({ plist_marshal_ops;plist_extra_ops; plist_ops }:(a,lwt)r1)
(**/**)

let make (type a) (x:a arg) : a res = x |> function
  | A1_elt_arb__lwt ({ max_elt_sz; m_elt; u_elt }, blk_dev_ops) -> 
    R1 (make_ ({ max_elt_sz; m_elt; u_elt }, blk_dev_ops))
  | A2_elt_int__lwt blk_dev_ops ->
    R2 (make_ (Marshal_factory.int_option_marshaller,blk_dev_ops))
