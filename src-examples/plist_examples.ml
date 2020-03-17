open Tjr_plist
open Plist_intf

(* FIXME use {blk_id} rather than an abstract type *)

let monad_ops = Tjr_monad.lwt_monad_ops

let ( >>= ) = monad_ops.bind

let return = monad_ops.return

module Test() : sig
  val main : unit -> (unit, lwt) m
end = struct

  (* open Buf_as_bigarray *)
  type blk_id_ = Blk_id_as_int.blk_id

  type map_ = (blk_id_,ba_buf)Tjr_map.With_stdcmp.stdmap

  let b2i blk_id = Blk_id_as_int.to_int blk_id
  (* let i2b i = Blk_id_as_int.of_int i *)

  let empty: map_ = 
    Tjr_map.With_stdcmp.empty ()

  (** In-mem blk dev, mutated directly *)
  let mem = ref empty

  let with_mem : ('a,lwt)with_state = 
    let with_state f = f ~state:!mem ~set_state:(fun s -> mem:=s; return ()) in
    { with_state }

  let _ = with_mem

  let blk_dev_ops with_map = 
    Blk_dev_factory.(make_4 with_map)

  let blk_dev_ops = blk_dev_ops with_mem

  let _ : (blk_id_,ba_buf,lwt) blk_dev_ops = blk_dev_ops

  let Plist_factory.{plist_extra_ops;plist_ops=mk_plist_ops;_} =  Plist_factory.make_2 blk_dev_ops

  let {create_plist;read_plist;read_plist_tl=_} = plist_extra_ops
    
  (* let plist_ops = plist_ops with_mem *)

  (* let _ = 
   *   Int_plist.check_blk := (fun blk -> let _ = m_u_ops.unmarshal blk in true) *)

  let num_elts = 10_000

  let write_ints ~plist_ops ~min_free_blk_id = (
    let {add;sync_tl;_} = plist_ops in
    (min_free_blk_id,0) |> iter_k (fun ~k (min_free_blk_id,n) -> 
        match n >= num_elts with
        | true -> 
          (* NOTE make sure the last block is actually written *)
          sync_tl () >>= fun () ->
          return min_free_blk_id
        | false -> 
          add
            ~nxt:(Blk_id_as_int.of_int min_free_blk_id)
            ~elt:n
          >>= fun x -> 
          match x with 
          | None -> k (min_free_blk_id+1,n+1)
          | Some _ -> k (min_free_blk_id,n+1)))

  let create_plist blk_id =
    create_plist blk_id >>= fun plist -> 
    let state = ref plist in
    let with_state : ((int, blk_id_, ba_buf) plist,lwt) with_state = 
      let with_state f = f ~state:!state ~set_state:(fun s -> 
          state:=s; return ())
      in
      {with_state}
    in
    return with_state

  let debug with_state = 
    with_state.with_state (fun ~state ~set_state ->
        let { hd; tl; blk_len; _ } = state in        
        Printf.printf "hd:%d tl:%d blk_len:%d \n" (b2i hd) (b2i tl) blk_len;
        return ())

  let test_1 () = 
    Printf.printf "# test 1\n";
    create_plist (Blk_id_as_int.of_int 0) >>= fun with_state -> 
    let plist_ops = mk_plist_ops with_state in
    (* NOTE min_free_blk_id is 1! *)
    write_ints ~plist_ops ~min_free_blk_id:1 >>= fun min_free_blk_id -> 
    Printf.printf "Finished with next free blk: %d\n" min_free_blk_id;

    (* read back and print len *)
    debug with_state >>= fun () ->
    read_plist (Blk_id_as_int.of_int 0) >>= fun xs ->
    xs |> List.map (fun (elts,nxt_) -> elts) |> List.concat |> fun xs ->
    Printf.printf "Read %#d elts from disk\n" (List.length xs);
    assert(num_elts = List.length xs);

    (* print some info *)
    let { get_hd; get_tl; blk_len; read_hd; adv_hd; adv_tl;_ } = plist_ops in
    get_hd () >>= fun hd ->
    get_tl () >>= fun tl ->
    blk_len () >>= fun len ->
    read_hd () >>= fun (elts,Some nxt) -> 
    Printf.printf "hd:%d tl:%d blk_len:%d hd.nxt:%d\n" (b2i hd) (b2i tl) len (b2i nxt);

    (* adv_hd and print some info *)
    adv_hd () >>= fun _ ->
    get_hd () >>= fun hd ->
    get_tl () >>= fun tl ->
    blk_len () >>= fun len ->
    read_hd () >>= fun (elts,Some nxt) -> 
    Printf.printf "After adv_hd: \n";
    Printf.printf "hd:%d tl:%d blk_len:%d hd.nxt:%d\n" (b2i hd) (b2i tl) len (b2i nxt);
    return ()[@@warning "-8"]

  let test_2 () = 
    Printf.printf "# test 2\n";
    
    (* create 1st plist and write some ints *)
    create_plist (Blk_id_as_int.of_int 0) >>= fun with_state -> 
    let plist_ops_1 = mk_plist_ops with_state in
    write_ints ~plist_ops:plist_ops_1 ~min_free_blk_id:1 >>= fun min_free_blk_id -> 
    Printf.printf "Finished with next free blk: %d\n" min_free_blk_id;

    (* create 2nd plist and write some ints *)
    (* assumes min_free_blk_id < 100 *)
    assert(min_free_blk_id < 100);
    create_plist (Blk_id_as_int.of_int 100) >>= fun with_state -> 
    let with_state_2 = with_state in
    let plist_ops_2 = mk_plist_ops with_state in
    (* NOTE min_free_blk_id is 101 *)
    write_ints ~plist_ops:plist_ops_2 ~min_free_blk_id:101 >>= fun min_free_blk_id -> 
    Printf.printf "Finished with next free blk: %d\n" min_free_blk_id;

    (* now join the two lists together, keeping list 1 *)
    with_state_2.with_state (fun ~state ~set_state ->
        return state) >>= fun pl2 ->
    plist_ops_1.append pl2 >>= fun () ->
    Printf.printf "Appended two plists\n";

    (* read back and check *)
    read_plist (Blk_id_as_int.of_int 0) >>= fun xs ->
    xs |> List.map (fun (elts,nxt_) -> elts) |> List.concat |> fun xs ->
    Printf.printf "Read %#d elts from disk\n" (List.length xs);
    assert(num_elts * 2 = List.length xs);
    let { get_hd; get_tl; blk_len; read_hd; adv_hd; adv_tl;_ } = plist_ops_1 in
    let b2i blk_id = Blk_id_as_int.to_int blk_id in
    get_hd () >>= fun hd ->
    get_tl () >>= fun tl ->
    blk_len () >>= fun len ->
    read_hd () >>= fun (elts,Some nxt) -> 
    Printf.printf "hd:%d tl:%d blk_len:%d hd.nxt:%d\n" (b2i hd) (b2i tl) len (b2i nxt);
    adv_hd () >>= fun _ ->
    get_hd () >>= fun hd ->
    get_tl () >>= fun tl ->
    blk_len () >>= fun len ->
    read_hd () >>= fun (elts,Some nxt) -> 
    Printf.printf "After adv_hd: \n";
    Printf.printf "hd:%d tl:%d blk_len:%d hd.nxt:%d\n" (b2i hd) (b2i tl) len (b2i nxt);
    return ()[@@warning "-8"]


  let main () = test_1 () >>= fun _ -> test_2 ()

  let _ = main
end

