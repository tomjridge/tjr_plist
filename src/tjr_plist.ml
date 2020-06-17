(** A persistent (on-disk), concurrent-safe (multiple writers are allowed, but only a single thread interacts with disk) list. 

{%html:
<img width='100%' src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">
%}

Look at {!Plist_intf} for the main types. 

Sync behaviour: There are at least 2 choices (in addition to
respecting explicit syncs):

- sync on every add

- sync on moving to a new tl

The current implementation syncs when moving to a new tail, not on every add. The sequence of operations is:

- Allocate new tail block and initialize it; sync 
- Update current tail block with next pointer to new tail; sync
- Update in-memory state so that the new tail becomes the "current" tail


*)

include Summary

module Plist_intf = Plist_intf

(* $(FIXME("prefer to access the roots directly rather than hooking?")) *)
module Plist_sync_root_blk = Plist_sync_root_blk


module Make_1 = Make_1

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

