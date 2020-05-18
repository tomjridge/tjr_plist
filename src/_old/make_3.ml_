(** Like {!Make_1}, but using objects etc to make the interface
   clearer to read *)

open Plist_intf

class type ['a, 'blk_id, 'blk, 'buf] mrshl_inf = object
  method plist_marshal_info: ('a, 'blk_id, 'blk, 'buf) plist_marshal_info 
  method buf_ops: 'buf buf_ops
  method blk_ops: 'blk blk_ops
end

class type ['blk_id, 'blk, 't] blk_dev = object
  method monad_ops : 't monad_ops
  method blk_dev_ops: ('blk_id, 'blk, 't) blk_dev_ops
end

class type ['blk_id, 'buf, 't] w_state = object
  method with_state : (('blk_id,'buf)plist,'t) with_state
end


let make (x:('a,'blk_id,'blk,'buf)mrshl_inf) = 
  Make_1.make ~plist_marshal_info:x#plist_marshal_info
    ~buf_ops:x#buf_ops
    ~blk_ops:x#blk_ops
  |> fun (plist_marshal_ops, `K1 rest) ->
  object
    method plist_marshal_ops=plist_marshal_ops
    method rest=(fun (arg2:(_,_,'t)blk_dev) ->
        rest ~monad_ops:arg2#monad_ops
          ~blk_dev_ops:arg2#blk_dev_ops
        |> fun (plist_extra_ops, `K2 rest) -> 
        object
          method plist_extra_ops=plist_extra_ops
          method rest=(fun (arg3:(_,_,_)w_state) -> 
              rest ~with_state:arg3#with_state)
        end)
  end

let _ = make

