TMP_DOC_DIR:=/tmp/tjr_plist
scratch:=/tmp/l/github/scratch

default: all

-include Makefile.ocaml

run:
	rm -f plist_on_disk.store
	time $(DUNE) exec plist_test

# for emacs completion
promote_docs::

# for auto-completion of Makefile target
clean::
