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


~~~
make -k run 
time dune exec plist_test
Finished with next free blk: 60
Read 10000 elts from disk
Finished with next free blk: 10
Read 10000 elts from disk
Timings: p1:3738187 p2:1020253

real	0m0.089s
user	0m0.073s
sys	0m0.015s
~~~


* OCamldoc: <https://tomjridge.github.io/tjr_plist/tjr_plist/index.html>
