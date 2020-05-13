(** A persistent (on-disk) list. 

{%html:
<img src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=645&amp;h=345">
%}


Look at {!Plist_intf} for the main types. {!Make_4} has the most succinct "make" function.
*)

module Plist_intf = Plist_intf
(* open Plist_intf *)
(* include Plist_intf *)

module Plist_sync_root_blk = Plist_sync_root_blk

module Make_1 = Make_1

(** NOTE hidden odoc for [Make_3,Make_4] *)

(**/**)
module Make_3 = Make_3

module Make_4 = Make_4
(**/**)

module Make_5 = Make_5


module Plist_factory = Plist_factory


(** NOTE hidden doc for module [Pl_type_abbrevs] *)

(**/**)
module Pl_type_abbrevs = Pl_type_abbrevs
(**/**)

