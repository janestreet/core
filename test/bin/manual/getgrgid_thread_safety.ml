(**
   This test reproduces a segfault in stdlib implementation of [getgrgid] in OCaml 4.07.
   Usually it segfaults in several seconds, but sometimes it can takes tens of seconds.

   getent group | cut -d ':' -f 3 | ./getgrgid_thread_safety.exe
*)

module List = struct
  include List

  let init n ~f =
    let rec go i acc =
      if (=) i n
      then
        List.rev acc
      else
        go (i + 1) (f i :: acc)
    in
    go 0 []
end

let () = Printf.printf "minor heap size: %d\n" ((Gc.get ()).minor_heap_size);;

let spawn i =
  ignore (
    Thread.create
      (fun () ->
         let rec f () =
           let g = (Core.Unix.Group.getbygid_exn i) in
           let len = Array.length g.mem in
           (* to test stdlib instead of core:
              [let len = Array.length ((Unix.getgrgid i).gr_mem) in] *)
           Printf.printf "%d\n%!" len;
           Thread.yield ();
           f () in
         f ())
      ())

let rec exp2 n = if n == 0 then 1 else 2 * exp2 (n - 1)

let alloc n =
  ignore (List.init (exp2 n) ~f:(fun x -> x))

let add_finaliser n =
  Gc.finalise (function
    | None -> assert false
    | Some n ->
      Printf.printf "finalising %d\n%!" n;
      alloc n; ())
    (Some n)

let finalise_spammer_thread () =
  let rec go () =
    ignore (List.init 21
              ~f:(fun i ->
                List.init 10 ~f:(fun _ ->
                  List.init i
                    ~f:(fun j ->
                      alloc j;
                      add_finaliser j))));
    Unix.sleep 10;
    go ()
  in
  go ()

let all_group_ids =
  let rec go acc =
    match read_int () with
    | exception End_of_file -> acc
    | x -> go (x :: acc)
  in
  go []

let () =
  (* It's good if the groups here are of very different sizes.

     If the groups have the same size, then you'll still get meaningless results
     due to races, but you probably won't segfault. *)
  let all_groups =
    List.map
      (fun i -> try Some (i, Array.length ((Unix.getgrgid i).gr_mem)) with
         | _ -> None)
      all_group_ids
  in
  let mx (i, iv) (j, jv) = if iv > jv then (i, iv) else (j, jv) in
  let mn (i, iv) (j, jv) = if iv < jv then (i, iv) else (j, jv) in
  let all_groups = List.concat (List.map (function
    | None -> []
    | Some x -> [x]) all_groups) in
  let (group_min, group_min_size) =
    List.fold_left
      mn
      (0, 1000000)
      all_groups
  in
  let (group_max, group_max_size) =
    List.fold_left
      mx
      (0, 0)
      all_groups
  in
  let (group_average, group_average_size) =
    let measure (_, sz) =
      sz * (group_max_size - sz)
    in
    List.fold_left
      (fun x y -> if measure x > measure y then x else y)
      (0, 0)
      all_groups
  in
  Printf.printf "large group: %d (%d)\n%!" group_max group_max_size;
  Printf.printf "small group: %d (%d)\n%!" group_min group_min_size;
  Printf.printf "average group: %d (%d)\n%!" group_average group_average_size;
  spawn group_min;
  spawn group_max;
  spawn group_average;
  ignore (Thread.create finalise_spammer_thread ());
  let rec loop () =
    Unix.sleep 100000;
    loop ()
  in
  loop ()
