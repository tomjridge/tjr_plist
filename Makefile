default: 
	$(MAKE) all

-include Makefile.ocaml

all::
	$(MAKE) copy_executables

# we need to copy the executables into this directory
copy_executables:
	$(DUNE) exec cp `which freelist_test` ${CURDIR}

# run_plist_test:
# 	rm -f plist_on_disk.store
# 	time $(DUNE) exec plist_test

run_freelist_test:
	rm -f freelist_example.store
#	./freelist_test 1
	./freelist_test 2

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
	rm -f *.store
