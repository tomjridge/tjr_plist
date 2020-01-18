open Tjr_plist

type blk_id = Blk_id_as_int.blk_id
type blk = bytes
type buf = Buf_as_bytes.by_buf

let make = make

let monad_ops = Tjr_monad.lwt_monad_ops

let buf_ops = Buf_as_bytes.by_buf_ops

(* FIXME rename Common_blk_ops so it starts with Blk_; also
   blk_as_string rather than string_blk_ops *)
let blk_ops = Tjr_fs_shared.Common_blk_ops.bytes_blk_ops

let blk_dev_ops fd = 
  Blk_dev_factory.(make A2_bytes_4096_lwt)
  |> function | (R2 k) -> k fd
              | _ -> failwith __LOC__

let m_any max_sz (x:'a) (buf,off) = 
  let n : int = Marshal.to_buffer buf off max_sz x [] in 
  (buf,off+n)

let u_any buf off : 'a * int = 
  let i : 'a = Marshal.from_bytes buf off in
  (* FIXME Marshal should have a from_buffer which returns value and number of bytes consumed *)
  let n = Marshal.total_size buf off in
  (i,off+n)


module Int_plist = struct
  (* FIXME these max sizes are guessed upper bounds *)
  let marshal_info : (int,blk_id,blk,buf)plist_marshal_info = {
    max_elt_sz=10;
    max_blk_id_sz=10;
    m_elt=m_any 10;
    u_elt=u_any;
    m_blk_id=m_any 10;
    u_blk_id=u_any;
    blk_to_buf=(fun x -> x);
    buf_to_blk=(fun x -> x);
  }

  let make fd = 
    let blk_dev_ops = blk_dev_ops fd in
    make ~monad_ops ~buf_ops ~blk_ops ~blk_dev_ops ~marshal_info

  let _ = make
end
