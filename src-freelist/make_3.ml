(** Like {!Make_2}, but combined with plist construction. *)

open Make_2

open Make_4

let make x = 
  Make_2.make (object
    method plist=(
      Tjr_plist.Make_4.make ~plist_marshal_info:(x#plist_marshal_info)
      |> fun (plist_marshal_ops,`K1 rest) -> 
      rest ~blk_dev_ops:(x#blk_dev_ops)
      |> fun (plist_extra_ops,`K2 rest) -> 
      rest ~with_state:(x#with_plist) |> fun plist_ops ->
      plist_ops)
    method root_block=(x#root_block)
    method version=(x#version)
    method with_freelist=(x#with_freelist)
  end)

let make : < blk_dev_ops : std_blk_dev_ops;
  plist_marshal_info : 'a plist_marshal_info;
  root_block : root_block_ops;
  version : ('a, r,t)version;
  with_freelist : 'a with_freelist;
  with_plist : (plist, t) with_state;
  > 
->
'a freelist_ops
  = make
(** {[
let make : < blk_dev_ops : std_blk_dev_ops;
  plist_marshal_info : 'a plist_marshal_info;
  root_block : root_block_ops;
  version : ('a, r,t)version;
  with_freelist : 'a with_freelist;
  with_plist : (plist, t) with_state;
  > 
->
'a freelist_ops
  = make
]}*)
