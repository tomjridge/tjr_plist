(** Common plist marshalling parameters *)

(** NOTE unless otherwise specified, take blk_id to be int and blk_sz to be 4096. NOTE
   bin_prot favours bigarray as the buffer type *)

type 'a arg = 
  (* | A1_elt_int__blk_string__buf_bytes *)
  | A2_elt_int__blk_ba_buf__buf_ba_buf
  | A3_elt_arbitrary__blk_ba_buf__buf_ba_buf of { 
      max_elt_sz:int;
      m_elt : 'a option -> (ba_buf * int) -> ba_buf * int;
      u_elt : ba_buf -> int -> 'a option * int }
(** 
- A3: elt is arbitrary; blk is bigarray; buf is bigarray; 
*)



type 'a res = 
  (* | R1 of (int,Blk_id_as_int.blk_id,string,bytes) plist_marshal_info *)
  | R2 of (int,Blk_id_as_int.blk_id,ba_buf,ba_buf) plist_marshal_info
  | R3 of ('a,Blk_id_as_int.blk_id,ba_buf,ba_buf) plist_marshal_info


module Int_option = struct
  open Bin_prot.Std
  type int_opt = int option [@@deriving bin_io]
end

let make = function
  | A2_elt_int__blk_ba_buf__buf_ba_buf -> 
    let module X = struct
      type blk_id = Blk_id_as_int.blk_id
      (* let b2i blk_id = Blk_id_as_int.to_int blk_id *)
      (* let i2b i = Blk_id_as_int.of_int i *)

      type blk = Buf_as_bigarray.ba_buf (* NOTE we must take care to copy the bytes when writing to disk *)

      (* let blk_ops = Blk_factory.(make A3_ba_4096 |> function R3 ops -> ops | _ -> failwith __LOC__) *)

      type buf = Buf_as_bigarray.ba_buf

      (* let buf_ops = Buf_as_bigarray.ba_buf_ops *)

      module Int_binprot = struct

        module Int_option = struct
          open Bin_prot.Std
          type int_opt = int option [@@deriving bin_io]
        end

        let max_sz = 10 (* 1 for option; 1 for int tag; 8 for int *)

        let m_elt (x:int option) (buf,off) = 
          Int_option.bin_write_int_opt buf ~pos:off x |> fun off' -> 
          (buf,off')

        let m_blk_id blk_id = 
          blk_id |> (function None -> None | Some x -> Some (Blk_id_as_int.to_int x))
          |> m_elt 

        let u_elt buf off = 
          let pos_ref = ref off in
          Int_option.bin_read_int_opt buf ~pos_ref |> fun r ->
          (r,!pos_ref)

        let u_blk_id buf off = 
          u_elt buf off |> fun (x,off) -> 
          x |> (function None -> None | Some x -> Some(Blk_id_as_int.of_int x)) |> fun x -> 
          (x,off)

        let marshal_info : (int,blk_id,blk,buf)plist_marshal_info = {
          max_elt_sz=max_sz;
          max_blk_id_sz=max_sz;
          m_elt;
          u_elt;
          m_blk_id;
          u_blk_id;
          blk_to_buf=(fun x -> Buf_as_bigarray.ba_copy x);
          buf_to_blk=(fun x -> Buf_as_bigarray.ba_copy x);
        }
      end
    end
    in
    R2 X.Int_binprot.marshal_info

  | A3_elt_arbitrary__blk_ba_buf__buf_ba_buf { max_elt_sz; m_elt; u_elt } -> 
    let module X = struct
      type blk_id = Blk_id_as_int.blk_id
      (* let b2i blk_id = Blk_id_as_int.to_int blk_id *)
      (* let i2b i = Blk_id_as_int.of_int i *)

      type blk = Buf_as_bigarray.ba_buf (* NOTE we must take care to copy the bytes when writing to disk *)

      (* let blk_ops = Blk_factory.(make A3_ba_4096 |> function R3 ops -> ops | _ -> failwith __LOC__) *)

      type buf = Buf_as_bigarray.ba_buf

      (* let buf_ops = Buf_as_bigarray.ba_buf_ops *)

      module Int_binprot = struct

        module Int_option = struct
          open Bin_prot.Std
          type int_opt = int option [@@deriving bin_io]
        end

        let max_sz = 10 (* 1 for option; 1 for int tag; 8 for int *)

        let m_blk_id blk_id = 
          blk_id |> (function None -> None | Some x -> Some (Blk_id_as_int.to_int x))
          |> m_elt 

        let u_blk_id buf off = 
          u_elt buf off |> fun (x,off) -> 
          x |> (function None -> None | Some x -> Some(Blk_id_as_int.of_int x)) |> fun x -> 
          (x,off)

        let marshal_info : (int,blk_id,blk,buf)plist_marshal_info = {
          max_elt_sz;
          max_blk_id_sz=max_sz;
          m_elt;
          u_elt;
          m_blk_id;
          u_blk_id;
          blk_to_buf=(fun x -> Buf_as_bigarray.ba_copy x);
          buf_to_blk=(fun x -> Buf_as_bigarray.ba_copy x);
        }
      end
    end
    in
    R3 X.Int_binprot.marshal_info

let _ = make
