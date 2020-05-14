(** A persistent (on-disk) list. 

{%html:
<img src="https://docs.google.com/drawings/d/e/2PACX-1vT1LGM8Sm7USD8LF_CGLUVZ270PK4vk5LcBrENxjcebpRUYq4jxPpgCTzNFsIS8TOgrcsVvcbZcNJ-M/pub?w=974&amp;h=871">
%}


Look at {!Plist_intf} for the main types. 

{[
type ('a,'buf (* FIXME *),'blk_id,(* 'blk,*) 't) plist_ops = {
  add       : nxt:'blk_id -> elt:'a -> ('blk_id option,'t) m;
  add_if_room: 'a -> (bool,'t)m;
  sync_tl   : unit -> (unit,'t)m;
  blk_len   : unit -> (int,'t)m;
  adv_hd    : unit -> ( ('a,'blk_id) adv_hd or_error,'t)m; 
  (** advance hd *)

  adv_tl    : 'blk_id -> (unit,'t)m;
  get_hd    : unit -> ('blk_id,'t)m;
  get_tl    : unit -> ('blk_id,'t)m;
  get_hd_tl : unit -> ('blk_id * 'blk_id,'t)m;
  read_hd   : unit -> ('a list * 'blk_id option,'t)m;
  append    : ('blk_id,'buf) plist -> (unit,'t)m;
}

type ('a,'blk_id,'blk,'buf,'t) plist_factory = <
  buf_ops            :'buf buf_ops;
  blk_ops            : 'blk blk_ops;
  plist_marshal_info : ('a,'blk_id,'blk,'buf) plist_marshal_info;
  plist_marshal_ops  : ('a,'blk_id,'blk) plist_marshal_ops; 
  with_blk_dev_ops   :  
    monad_ops   :'t monad_ops -> 
    blk_dev_ops : ('blk_id,'blk,'t)blk_dev_ops 
    -> <
      monad_ops       : 't monad_ops;
      blk_dev_ops     : ('blk_id,'blk,'t)blk_dev_ops;
      plist_extra_ops : ('a,'buf,'blk_id,'t) plist_extra_ops;
      with_state      : (('blk_id,'buf)plist,'t)with_state -> 
        ('a,'buf,'blk_id,'t)plist_ops;
      from_disk       : <hd:'blk_id;tl:'blk_id;blk_len:int> -> 
        (<
          plist_ops:('a,'buf,'blk_id,'t)plist_ops;
          with_plist: (('blk_id,'buf)plist,'t)with_state;
          plist_ref: ('blk_id,'buf)plist ref;              
        >,'t)m
    >
>

]}

*)

module Plist_intf = Plist_intf

(* $(FIXME("prefer to access the roots directly rather than hooking?")) *)
module Plist_sync_root_blk = Plist_sync_root_blk

(** NOTE hidden doc for [Make_1,Make_3,Make_4] *)

(**/**)
module Make_1 = Make_1

module Make_3 = Make_3

module Make_4 = Make_4
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
      let int_plist_factory = M5.plist_factory ~buf_ops ~blk_ops ~plist_marshal_info
    end)
  in
  object 
    method int_plist_factory : int plist_factory = int_plist_factory      
  end




(** NOTE hidden doc for modules [Pl_type_abbrevs,Plist_factory] *)
(* $(CONVENTION("Name a module like pl_type_abbrevs for local type
   abbrevs assuming lwt etc")) *)

(**/**)
module Pl_type_abbrevs = Pl_type_abbrevs

(* FIXME remove *)
module Plist_factory = Plist_factory

(**/**)

