(** More std type abbrevs *)

open Plist_intf

include Sh_std_ctxt

type nonrec 'a plist_marshal_info = ('a,blk_id,ba_buf,ba_buf)plist_marshal_info

type nonrec 'a plist_marshal_ops = ('a,blk_id,ba_buf)plist_marshal_ops

type nonrec 'a plist_extra_ops = ('a,ba_buf,blk_id,t)plist_extra_ops

type nonrec plist = (blk_id,ba_buf)plist

type nonrec 'a plist_ops = ('a,ba_buf,blk_id,t)plist_ops



