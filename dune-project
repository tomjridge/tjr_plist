(lang dune 2.0)
(name tjr_plist)

;; for opam
(authors "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers "Tom Ridge <tom.j.ridge@googlemail.com>")
(source (github tomjridge/tjr_plist))
(homepage https://github.com/tomjridge/tjr_plist)
(documentation https://tomjridge.github.io/tjr_plist)
(generate_opam_files true)


(package 
         (name tjr_plist)
         (synopsis "A persistent (on-disk) list")
         (depends ocamlfind dune tjr_fs_shared ppx_jane)
)


(package 
         (name tjr_plist_examples)
         (synopsis "A persistent (on-disk) list (examples)")
         (depends ocamlfind dune tjr_plist)
)


;; (package 
;;          (name mini_examples)
;;          (synopsis "FIXME")
;;          (depends mini)
;; )


