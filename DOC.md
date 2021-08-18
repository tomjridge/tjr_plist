# Documentation

This file explains the internal datastructures and operational semantics.

This is a persistent on-disk list. During operation we keep the following in memory:

```
type ('blk_id,'buf) plist = {
  hd      : 'blk_id;
  tl      : 'blk_id;
  buffer  : 'buf;
  off     : int;
  blk_len : int;
  tl_dirty: bool; (** tl may not have been written to disk *)
}
```


`hd` points to the list start block; `tl` points to the "current" block, where we write new elements; `buffer` is the current block in memory; `off` is an offset within buffer; `blk_len` is the length of the list on disk; `tl_dirty` records whether the in-memory buffer is dirty (ie needs to be written to disk at some point).

## Buffer layout

The buffer is laid out as follows:

```
[ <nxt?> | <elt 0> | ... | <elt n> | <None> | <remaining junk bytes within the block> ]
                                     ^off
```

Note `<elt 0>` is marshalled as `Some(e0)`.

`<nxt?>` is the optional next pointer.  The next pointer starts off as None, and is only set when moving to a new block. The `off` offset pointer points to the location of the `<None>` end-of-list segment.

## Adding an element and marshalling

The current approach assumes that we know the max size of a marshalled elt upfront (this should probably change, as JStreet bin_prot lib - just try to marshal, and throw exception if not enough space). Then, if we have enough space, adding an element just marshals to the buffer at `off`. Currently, we delay adding the `<None>` terminator till we sync to disk.

If there is not enough space, we move to a new tail, and write the element there. Moving to a new tail consists of:
* Allocating a new block; initializing it with the new elt
* Updating the nxt pointer in the old tail, and writing the old buffer to disk
* Switching the in-mem state to the new block

## Appending two persistent lists

We have two persistent lists: `pl1` and `pl2`. The append operation results in a list that starts from `pl1.hd` and terminates at `pl2.tl`, and uses the `pl1` in-memory state. To accomplish this:
* Set `pl1.nxt` to `pl2.hd`
* Update the `pl1` in-memory state to match `pl2` (set `pl1.tl` to `pl2.tl`; `buffer` to `pl2.buffer`; `off` to `pl2.off` etc.; obviously at this point pl2 ops and in-mem state should not be used)



