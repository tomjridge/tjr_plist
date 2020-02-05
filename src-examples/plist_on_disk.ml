open Bin_prot.Std

type blk_id = int[@@deriving yojson, bin_io]

type root_blk = {
  hd:blk_id;
  tl:blk_id;
  min_free_blk_id:blk_id
}[@@deriving yojson, bin_io]
  

let init_root_blk = { hd=1;tl=1;min_free_blk_id=2 }

let init_blks = 
  (0,init_root_blk),
  (1,"empty_plist")

let blk_dev' = Blk_dev_factory.(make A1_string_4096_lwt_fd |> fun (R1 f) -> f)[@@warning "-8"]


(* let create  *)
