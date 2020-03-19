(** Common plist marshalling parameters *)

(** NOTE unless otherwise specified, take blk_id to be int and blk_sz to be 4096. NOTE
   bin_prot favours bigarray as the buffer type *)

open Plist_intf

(** Internal: blk and buf are ba_buf; ba4096 *)
module Internal_ = struct
  type blk_id = Blk_id_as_int.blk_id

  (* FIXME have a buf_factory *)
  module M3 = struct
    type buf = ba_buf
    let buf_ops = ba_buf_ops
    type blk = ba_buf
    (* NOTE we must take care to copy the ba_buf when writing to disk *)
    let blk_ops = Blk_factory.make_3 ()
  end

  module Int_option = struct
    open Bin_prot.Std
    type int_opt = int option [@@deriving bin_io]
  end
end
open Internal_  

type 'a arg = 
  (* | A1_elt_int__blk_string__buf_bytes *)
  | A2_elt_int__blk_ba_buf__buf_ba_buf
  | A3_elt_arbitrary__blk_ba_buf__buf_ba_buf of 'a arg' 
and 'a arg' = ('a option,ba_buf)mshlr
(** 
- A2: elt is int
- A3: elt is arbitrary
*)

type 'a res = 
  (* | R1 of (int,Blk_id_as_int.blk_id,string,bytes) plist_marshal_info *)
  | R2 of int res'
  | R3 of 'a res'
and 'a res' = {
  plist_marshal_info: ('a,blk_id,M3.blk,M3.buf) plist_marshal_info;
  buf_ops: M3.buf buf_ops;
  blk_ops: M3.blk blk_ops
}

(**/**)
let make_ (type a) (elt_mshlr:a arg') =
  let module X = struct
    (* let b2i blk_id = Blk_id_as_int.to_int blk_id *)
    (* let i2b i = Blk_id_as_int.of_int i *)

    type blk = M3.blk 
    let blk_ops = M3.blk_ops

    type buf = M3.buf
    let buf_ops = M3.buf_ops
                    
    let int_opt_mshlr = Marshal_factory.make_1

    let max_blk_id_sz = int_opt_mshlr.max_elt_sz
      
    let m_blk_id blk_id = 
      blk_id |> (function None -> None | Some x -> Some (Blk_id_as_int.to_int x))
      |> int_opt_mshlr.mshl

    let u_blk_id buf off = 
      int_opt_mshlr.umshl buf off |> fun (x,off) -> 
      x |> (function None -> None | Some x -> Some(Blk_id_as_int.of_int x)) |> fun x -> 
      (x,off)

    (* FIXME move to common marshal_factory *)
    let blk_id_mshlr = { max_elt_sz=max_blk_id_sz; mshl=m_blk_id; umshl=u_blk_id }

    let plist_marshal_info : (a,blk_id,blk,buf)plist_marshal_info = {
      elt_mshlr;
      blk_id_mshlr;
      blk_to_buf=(fun x -> Buf_factory.Buf_as_bigarray.ba_copy x);
      buf_to_blk=(fun x -> Buf_factory.Buf_as_bigarray.ba_copy x);
    }
  end
  in
  X.{ plist_marshal_info; buf_ops; blk_ops }
(**/**)

let _ = make_

let make_2 =
  let x = Marshal_factory.make_1 in
  make_ x 

let make_3 (type a) (x:a arg') = make_ x

(*
let make (type a) (x:a arg) : a res = x |> function
  | A3_elt_arbitrary__blk_ba_buf__buf_ba_buf x -> 
    R3 (make_ x)
  | A2_elt_int__blk_ba_buf__buf_ba_buf -> 
    let x = Marshal_factory.make_1 in
    make_ x |> fun x -> R2 x[@@warning "-8"]

let _ = make
*)
