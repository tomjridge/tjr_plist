(** A persistent (on-disk) list. The freelist based on this is 
concurrent-safe (multiple writers are allowed, but only a single thread interacts with disk), but this code is not.

NOTE the freelist code has moved to the imp_fs repo.

{%html:
<img width='100%' src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">
%}

Look at {!Plist_intf} for the main types. 

A persistent list is an on-disk list, where elements can be added at
the tail. The "origin block" stores the hd blkid, the tl blkid and the
length of the list. From this, the in-memory state of the list can be
reconstructed after a system crash. The hd is expected not to change
(much). The tl may change (as new elts get added and the tl block gets
full, the tl advances) but since we can follow links from the hd (or
the old tl) we don't have to modify the origin block when the tl
changes. Thus, we can add elements to the end of the list, and make
the changes persistent with a single sync.

We also have an additional operation of "adv_hd", which moves the hd
pointer to the next blk. This does require an update to the origin
block before the new hd is made visible to the rest of the system.

The adv_hd operation is used by the freelist. For the freelist, each
block on disk stores a list of (free) blkids. To avoid repeated disk
access, free blkids are typically transferred to memory as a batch
using a single blk read (via the adv_hd operation).
Sync behaviour: There are at least 2 choices (in addition to
respecting explicit syncs):

- sync on every add
- sync on moving to a new tl

The current implementation syncs when moving to a new tail, not on
every add. Thus, some elements that have been added, but not synced,
may be lost on a system crash. If needed, you can avoid this by
explicitly syncing after each add (with the commensurate loss of
performance). The sequence of operations is:

- Allocate new tail block and initialize it; sync 
- Update current tail block with next pointer to new tail; sync
- Update in-memory state so that the new tail becomes the "current" tail


*)

include Summary

module Plist_intf = Plist_intf

module Pl_origin = Plist_intf.Pl_origin

module Plist_ops = Plist_intf.Plist_ops
type ('a,'buf, 'blk_id, 't) plist_ops =
  ('a,'buf, 'blk_id, 't) Plist_intf.plist_ops

type ('a,'blk_id,'blk,'buf,'t) plist_factory
  = ('a,'blk_id,'blk,'buf,'t) Plist_intf.plist_factory

type ('blk_id,'t) freelist_ops_af =
  ('blk_id,'t) Plist_intf.freelist_ops_af

type ('a,'blk_id,'blk,'buf,'t) simple_plist_factory
  = ('a,'blk_id,'blk,'buf,'t) Plist_intf.simple_plist_factory

module Simple_plist_ops = Plist_intf.Simple_plist_ops
type ('a,'blk_id,'t) simple_plist_ops = 
  ('a,'blk_id,'t) Plist_intf.simple_plist_ops

module Make_1 = Make_1

(* module Make_5 = Make_5 *)

(* $(CONVENTION("Place common examples under eg Tjr_plist.pl_examples
   object; this includes std_ctxt-specific type instances eg
   origin_mshlr for blk_id = int ")) *)

let pl_examples = Make_1.pl_examples


module Make_simple_plist = Make_2



let simple_pl_examples = Make_2.examples


(** NOTE hidden doc for module [Pl_type_abbrevs] *)
(* $(CONVENTION("Name a module like pl_type_abbrevs for local type
   abbrevs assuming lwt etc")) *)

(**/**)
module Pl_type_abbrevs = Pl_type_abbrevs
(**/**)

