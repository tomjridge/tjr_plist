default: 
	$(MAKE) all

-include Makefile.ocaml

run:
	rm -f plist_on_disk.store
	time $(DUNE) exec plist_test

run_freelist_test:
	rm -f freelist_example.store
	time $(DUNE) exec freelist_test

update_generated_doc::
	cd src && (ocamldoc_pyexpander plist_intf.ml)
	cd src && (ocamldoc_pyexpander summary.t.ml > summary.ml)
	cd src-freelist && (ocamldoc_pyexpander freelist_intf.ml)
	cd src-freelist && (ocamldoc_pyexpander summary.t.ml > summary.ml)

	$(MAKE) docs

# for emacs completion
promote_docs::

# for auto-completion of Makefile target
clean::
