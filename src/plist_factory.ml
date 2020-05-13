(** {2 Plist factory: simple instances} *)

open Plist_intf
open Pl_type_abbrevs

let int_marshal_info: int plist_marshal_info = {
  elt_mshlr=Marshal_factory.make_1;
  blk_id_mshlr=Marshal_factory.make_2;
  blk_to_buf=blk_to_buf;
  buf_to_blk=buf_to_blk;
}
  
let int_plist_ops = Make_4.(
    make ~plist_marshal_info:int_marshal_info
    |> fun (plist_marshal_ops,`K1 rest) ->
    (plist_marshal_ops,`K1 (fun ~(blk_dev_ops:blk_dev_ops') -> 
         rest ~blk_dev_ops |> fun (plist_extra_ops,`K2 rest) ->
         (plist_extra_ops,`K2 (fun ~with_state -> rest ~with_state)))))

let int_plist_ops: 
int Make_4.plist_marshal_ops *
[> `K1 of
     blk_dev_ops:blk_dev_ops' ->
     int Make_4.plist_extra_ops *
     [> `K2 of
          with_state:(Make_4.plist, Make_4.t)
                     with_state ->
          int Make_4.plist_ops ] ]
= int_plist_ops
(** {[
int Make_4.plist_marshal_ops *
[> `K1 of
     blk_dev_ops:Make_4.std_blk_dev_ops ->
     int Make_4.plist_extra_ops *
     [> `K2 of
          with_state:(Make_4.plist, Make_4.t) with_state ->
          int Make_4.plist_ops ] ]
]} *)
