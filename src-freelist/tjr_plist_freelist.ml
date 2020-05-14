(** A freelist providing alloc and free, based on plist; safe to open this module. 

{%html:
<img src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">

%}


*)


module Freelist_intf = Freelist_intf

(** {2 Make functors} *)

module Fl_make_1 = Fl_make_1

module Fl_make_2 = Fl_make_2

module Fl_make_3 = Fl_make_3


(** {2 Example and summary} *)

module Freelist_example = Freelist_example


module Summary = Summary
