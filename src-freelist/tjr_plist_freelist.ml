(** A freelist providing alloc and free, based on plist;
   concurrent-safe ((multiple writers are allowed, but only a single
   thread interacts with disk); safe to open this module.

{%html: <img width='100%'
   src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">

%}

*)

include Summary


module Freelist_intf = Freelist_intf

(** {2 Make functors} *)

module Fl_make_1 = Fl_make_1


(** {2 Examples and summary} *)

let fl_examples = Fl_make_2.fl_examples

module Summary = Summary
