(** More std type abbrevs *)

open Plist_intf

include Shared_ctxt

type nonrec 'a plist_marshal_info = ('a,blk_id,buf)plist_marshal_info

type nonrec 'a plist_marshal_ops = ('a,blk_id,buf)plist_marshal_ops

(* type nonrec 'a plist_extra_ops = ('a,ba_buf,blk_id,t)plist_extra_ops *)

type nonrec plist = (blk_id,buf)plist

type nonrec 'a plist_ops = ('a,buf,blk_id,t)plist_ops

type nonrec 'a plist_factory = ('a,blk_id,blk,buf,t)plist_factory



