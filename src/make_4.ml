(** Like {!Make_1} but with stdtypes. 

odoc documentation not great on this module - prefer #show in top-level. *)

(* open Plist_intf *)

(* open Plist_intf.Std_types *)

(** NOTE hidden include Plist_intf.Std_types, to make odoc output readable *)
(**/**)
include Plist_intf.Std_types
(**/**)

let make 
    ~(plist_marshal_info:'a plist_marshal_info)
  =
  Make_1.make ~plist_marshal_info ~buf_ops ~blk_ops |> fun (plist_marshal_ops, `K1 rest) ->
  ((plist_marshal_ops:'a plist_marshal_ops), `K1(fun ~(blk_dev_ops:std_blk_dev_ops) ->
       rest ~monad_ops ~blk_dev_ops |> fun (plist_extra_ops, `K2 rest) ->
       ((plist_extra_ops:'a plist_extra_ops), `K2(fun ~(with_state:(plist,t)with_state) -> 
            rest ~with_state |> fun (x:'a plist_ops) -> 
            x))))

(*
  object
    method plist_marshal_ops=( (x#plist_marshal_ops): 'a plist_marshal_ops)
    method rest=(fun ~(blk_dev_ops:std_blk_dev_ops) ->
        x#rest (object
          method monad_ops=monad_ops
          method blk_dev_ops=blk_dev_ops
        end) |> fun x ->
        object
          method plist_extra_ops=( (x#plist_extra_ops):'a plist_extra_ops)
          method rest=(fun ~(with_state: (plist,t) with_state) -> 
              let r : 'a plist_ops = x#rest (object method with_state=with_state end) in
            r)
        end)
  end

let _ = make
            
**)
