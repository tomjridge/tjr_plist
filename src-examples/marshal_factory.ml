(** Common marshallers *)

(* FIXME this needs to move to fs_shared.examples *)

(* FIXME this is a generic type, move to fs_shared *)

(** Generic marshaller type, assuming max_elt_sz is known (alternative
   is to marshal and check for end of buffer explicitly) *)
type ('a,'buf) marshaller = {
  max_elt_sz: int;
  m_elt: 'a -> ('buf * int) -> 'buf * int;
  u_elt: 'buf -> int -> 'a * int 
}


(** Internal: buf is ba_buf *)

module Internal = struct
  type buf = ba_buf
end
open Internal

type arg = 
  | A1_int_option__ba_buf

type res =
  | R1 of (int option,buf) marshaller

let make = function
  | A1_int_option__ba_buf -> 
    let module Int_option = struct
      open Bin_prot.Std
      type int_opt = int option [@@deriving bin_io]
    end
    in
    let module X = struct
      let max_elt_sz = 10

      let m_elt (x:int option) (buf,off) = 
        Int_option.bin_write_int_opt buf ~pos:off x |> fun off' -> 
        (buf,off')

      let u_elt buf off = 
        let pos_ref = ref off in
        Int_option.bin_read_int_opt buf ~pos_ref |> fun r ->
        (r,!pos_ref)

      let x = { max_elt_sz; m_elt; u_elt }
    end
    in
    R1 X.x

(** Convenience *)
let int_option_marshaller = make (A1_int_option__ba_buf) |> fun (R1 x) -> x
