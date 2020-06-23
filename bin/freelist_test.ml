(** Test freelist functionality *)


open With_lwt

let _ = 
  Lwt_main.run @@ Tjr_monad.With_lwt.to_lwt begin
    Freelist_example_1.run_example 
      ~params:(object
        method filename = "freelist_test_1.store"
      end)
      () >>= fun () -> 

    Freelist_example_2.run_example
      ~params:(object
        method filename = "freelist_test_2.store"
        method min_free_alloc_size = 1000
        method tr_lower=2
        method tr_upper=4
      end)
      ()
  end
