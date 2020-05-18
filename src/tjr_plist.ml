(** A persistent (on-disk) list. 

{%html:
<img width='100%' src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">
%}

Look at {!Plist_intf} for the main types. 

Sync behaviour: There are at least 3 choices (in addition to
respecting explicit syncs):

- sync on every add

- sync on moving to a new tl; if we assume the blk_dev respects write
  order, we don't need to sync of course (we initialize new blk, write
  old blk with pointer, and advance to new); but this doesn't
  guarantee the writes are on the disk (just that the on-disk
  structure is consistent with some prior state)

The point of the plist is that we can move to a new tl blk without
necessarily rewriting any pointers outside the 2 plist tl blocks, but
the price is that we have to traverse the entire list when we recover.

For the cost of one extra block write (for 3 synchronized block writes), we can sync the plist root block as well (ie the block that record hd, tl and len).

The freelist goes a step further to try to minimize costly disk interaction: it maintains a set of free blks, and only when these get low does it (asynchronously) try to replenish from disk. The cost of this is that a crash will result in some orphaned/lost blocks that can only be recovered by an fsck-like scan or some other mechanism. However, the assumption is that crashes are rare, and that the blk device is large enough that some lost blocks are not too much of an issue.

*)

include Summary

module Plist_intf = Plist_intf

(* $(FIXME("prefer to access the roots directly rather than hooking?")) *)
module Plist_sync_root_blk = Plist_sync_root_blk

(** NOTE hidden doc for [Make_1] *)

(**/**)
module Make_1 = Make_1
(**/**)

module Make_5 = Make_5

(* $(CONVENTION("Place common examples under eg Tjr_plist.pl_examples object")) *)

let pl_examples = 
  let open Pl_type_abbrevs in
  let open (struct
    module S = struct 
      type nonrec buf = buf
      type nonrec blk_id = blk_id
      type nonrec blk = blk
      type nonrec t = t
    end
    module M5 = Make_5.Make(S)
    let plist_marshal_info: int plist_marshal_info = {
      elt_mshlr=Marshal_factory.make_1;
      blk_id_mshlr=Marshal_factory.make_2;
      blk_to_buf=blk_to_buf;
      buf_to_blk=buf_to_blk;
    }
    let int_plist_factory = M5.plist_factory ~monad_ops ~buf_ops ~blk_ops 
        ~plist_marshal_info
  end)
  in
  object 
    method int_plist_factory : int plist_factory = int_plist_factory      
  end




(** NOTE hidden doc for module [Pl_type_abbrevs] *)
(* $(CONVENTION("Name a module like pl_type_abbrevs for local type
   abbrevs assuming lwt etc")) *)

(**/**)
module Pl_type_abbrevs = Pl_type_abbrevs
(**/**)

