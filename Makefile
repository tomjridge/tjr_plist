default: 
	$(MAKE) all

-include Makefile.ocaml

all::
# FIXME uncomment these after src-example resurrected
#	dune build bin/plist_test.exe
#	cp _build/default/plist_test.exe .

# run_plist_test:
# 	rm -f plist_on_disk.store
# 	time $(DUNE) exec plist_test

# FIXME restore these test exes
# run_freelist_test:
# 	rm -f freelist_example.store
# #	./freelist_test 1
# 	./freelist_test 2

update_generated_doc::
	cd src && (ocamldoc_pyexpander plist_intf.ml)
	cd src && (ocamldoc_pyexpander make_2.ml)
	cd src && (ocamldoc_pyexpander summary.t.ml > summary.ml)
	$(MAKE) docs

# for emacs completion
promote_docs::

# for auto-completion of Makefile target
clean::
	rm -f *.store
	rm -f src/GEN.*
