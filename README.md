# tjr_plist, a persistent (on-disk) list (part of ImpFS)

The main functionality provided by this library is to maintain an
on-disk singly-linked list, where each block stores a list of
elements, and a pointer to the next blk (if any).


~~~
type ('a,'buf,'blk_id,'blk,'t) plist_ops = {
  add  : nxt:'blk_id -> elt:'a -> ('blk_id option,'t) m;
  sync : unit -> (unit,'t)m; 
}
~~~

Note that `add` takes a `blk_id` in case a new block needs to be added
at the end of the list. The blk id is returned if it isn't used.

The library `tjr_plist_examples` contains examples.

The executable `plist_test` tests basic functionality using OCaml
marshalling and an in-memory blk device.
