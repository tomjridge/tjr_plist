(** Like {!Freelist_make} but with [std_types] *)

(** NOTE hidden include Pl_type_abbrevs, to make odoc output readable *)

(**/**)
include Tjr_monad.With_lwt
open Freelist_intf
include Tjr_plist.Pl_type_abbrevs
include Freelist_make

let async f = from_lwt(Lwt.async (fun () -> to_lwt (f ())) |>fun () -> Lwt.return ()) (* FIXME move to With_lwt *)
let event_ops = lwt_event_ops (* FIXME rename in With_lwt *)
(**/**)

let make x : ('a,_) freelist_ops = 
  Freelist_make.make (object
    method async=async
    method event_ops=event_ops
    method monad_ops=monad_ops
    method plist : 'a plist_ops =(x#plist)
    method root_block : (_,_)root_block_ops =(x#root_block)
    method version : ('a,r,t) version =(x#version)
    method with_freelist : ('a freelist,t)with_state =(x#with_freelist)
  end)

let make : 
  < plist : 'a plist_ops; 
    root_block : (r,t)root_block_ops;
    version : ('a, r, t) version; 
    with_freelist : ('a freelist,t) with_state; > 
  ->
  ('a,t) freelist_ops
= make
(** {[
let make : 
  < plist : 'a plist_ops; 
    root_block : root_block_ops;
    version : ('a, r, t) version; 
    with_freelist : 'a with_freelist; > 
  ->
  'a freelist_ops
= make
]}*)
