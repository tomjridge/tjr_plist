# tjr_plist, a persistent (on-disk) list (part of ImpFS)

The main functionality provided by this library is to maintain an
on-disk singly-linked list, where each block stores a list of
elements, and a pointer to the next blk (if any).

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



## tjr_plist_freelist

This library uses the persistent list to implement an on-disk freelist. Some effort is taken to make this efficient eg a transient set of free elts is held in memory, and allocating from this set (which is the usual case) does not cause any disk activity.



## OCamldoc

A collection of ocamldocs is here: <https://tomjridge.github.io/ocamldocs>