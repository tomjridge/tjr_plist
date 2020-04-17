default: all

-include Makefile.ocaml

run:
	rm -f plist_on_disk.store
	time $(DUNE) exec plist_test

run_freelist_test:
	rm -f freelist_example.store
	time $(DUNE) exec freelist_test

# for emacs completion
promote_docs::

# for auto-completion of Makefile target
clean::
