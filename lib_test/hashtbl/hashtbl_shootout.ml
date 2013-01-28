open Jane.Std
open Core_extended.Std
open Core_experimental.Std

module type Test_intf = sig
  type ('k, 'v) t
  val create : ?hash:('k -> int) -> int -> ('k, 'v) t
  val find : ('k, 'v) t -> 'k -> 'v option
  val fold : ('k, 'v) t -> init:'c -> f:(key:'k -> data:'v -> 'c -> 'c) -> 'c
  val add : ('k, 'v) t -> key:'k -> data:'v -> ('k, 'v) t
end

module Of_table_new (T : Table_new_intf.Basic) : Test_intf = struct
  type ('k, 'v) t = ('k, 'v) T.t

  let create ?(hash = Table_new_intf.poly.Table_new_intf.hash) size =
    T.create ~params:(Table_new_intf.size size)
      { Table_new_intf.
        hash = hash;
        compare = Pervasives.compare }

  let find = T.find
  let fold = T.fold
  let add t ~key ~data = T.add t ~key ~data; t
end

module Of_map (T : Core.Core_map_intf.S2) : Test_intf = struct
  type ('k, 'v) t = ('k, 'v) T.t

  let create ?hash _size = let _ = hash in T.empty
  let find = T.find
  let fold t ~init ~f = T.fold t ~init ~f
  let add t ~key ~data = T.add t ~key ~data
end

module Std_hashtbl : Test_intf = struct
  module T = Hashtbl.Poly
  type ('k, 'v) t = ('k, 'v) T.binable

  let create ?hash size = let _ = hash in T.create ~size ()
  let find = Hashtbl.find
  let fold = Hashtbl.fold
  let add t ~key ~data = Hashtbl.replace t ~key ~data; t
end

module Random = struct
  include Random.State

  let string t l =
    let s = String.create l in
    for i = 0 to l - 1 do
      s.[i] <- Core.Std.Char.of_int_exn (int t 255)
    done;
    s
  ;;
end

module Strings = struct
  type t = {
    mutable next: int;
    set: string array;
    state: Random.t;
  }

  let create () =
    let t = Array.create 1_000_000 "" in
    (* This will always generate the same sequence of strings. This
       is important because we want test results to be reproducable *)
    let s = Random.make [|5|] in
    for i = 0 to 999_999 do
      let sz = 1 + Random.int s 20 in
      let s = Random.string s sz in
      t.(i) <- s
    done;
    { set = t; next = 0; state = s }

  let next t =
    if t.next < Array.length t.set then begin
      let s = t.set.(t.next) in
      t.next <- t.next + 1;
      s
    end else begin
      t.next <- 0;
      t.set.(t.next)
    end

  let random t = t.set.(Random.int t.state 1_000_000)

  (* force the processor to read lots of data into it's cache *)
  let bash_cache t =
    let z = ref 0 in
    for i = 0 to 10_000 do
      z := Core.Std.Char.to_int t.set.(i).[0]
    done
end

module Test_data = struct
  (* designed to cause heap fragmentation *)
  let s = Random.make [|10|]

  module Subtest = struct
    type t = {
      mutable a: [`R | `M of [`A of string | `B of int]];
      b: int;
      c: float;
      d: string;
      mutable e: int option;
    }

    let create () =
      { a =
          (if Random.bool s then `R
           else if Random.bool s then `M (`A (String.create 10))
           else `M (`B 9));
        b = 10;
        c = Random.float s 10_000.0;
        d = Random.string s 15;
        e = Some 50 }
  end

  type t = {
    a: string;
    b: int;
    c: float;
    mutable d: [`Z of string | `X];
    e: float * float;
    f: float;
    mutable g: Subtest.t option;
  }

  let create () =
    { a = Random.string s 10;
      b = 10;
      c = Random.float s 10_000_000.0;
      d = `Z (Random.string s 14);
      e = (Random.float s 100_000.0, Random.float s 100_000.0);
      f = Random.float s 100_000.0;
      g = Some (Subtest.create ()) }
end

module Stats = struct
  type t = {
    ord : Order_stats.t;
    rstats : read_write Rstats.t;
  }

  let create () =
    { ord = Order_stats.create ();
      rstats = Rstats.create () }

  let clear t =
    Order_stats.clear t.ord;
    Rstats.clear t.rstats

  let add_sample t sample =
    let sample = Float.max 0.0 sample in
    Order_stats.add t.ord sample;
    Rstats.update_in_place t.rstats sample

  let run t f =
    let st = Time.now () in
    let res = f () in
    let et = Time.now () in
    let elapsed = Time.diff et st in
    add_sample t (Time.Span.to_ms elapsed);
    res

  let print t =
    let get_float = function
      | Error _ -> failwith "Couldn't get float"
      | Ok f -> f
    in
    Printf.printf "%f,%f,%f,%f,%f,%f,%f,%f\n%!"
      (Rstats.mean t.rstats)
      (Rstats.max t.rstats)
      (Rstats.min t.rstats)
      (Rstats.stdev t.rstats)
      (get_float (Order_stats.percentile t.ord 0.25))
      (get_float (Order_stats.median t.ord))
      (get_float (Order_stats.percentile t.ord 0.75))
      (get_float (Order_stats.percentile t.ord 0.95))
end

module Make (Tbl : Test_intf) = struct
  let s = Random.make [|15|]

  let insert samples size n =
    let rs = Stats.create () in
    (* There is no better way to fragment the heap than with a map *)
    let m = ref Map.empty in
    let rec loop i tbl =
      if i < n then begin
        let key = String.copy (Strings.random samples) in
        let data = Test_data.create () in
        let tbl = Stats.run rs (fun () -> Tbl.add tbl ~key ~data) in
        m := Map.add !m ~key ~data;
        (* poke some nasty little holes *)
        data.Test_data.g <- Some (Test_data.Subtest.create ());
        data.Test_data.d <- `X;
        Strings.bash_cache samples;
        loop (i + 1) tbl
      end else
        tbl
    in
    let tbl = loop 0 (Tbl.create size) in
    (rs, tbl)

  let lookup samples tbl =
    let rs = Stats.create () in
    for i = 1 to 1_000_000 do
      let key = Strings.next samples in
      ignore (Stats.run rs (fun () -> Tbl.find tbl key));
      Strings.bash_cache samples;
    done;
    rs

  let worst_case_insert samples =
    let rs = Stats.create () in
    let rec loop i tbl =
      if i < 500_000 then begin
        let key = String.copy (Strings.random samples) in
        let data = Test_data.create () in
        let tbl = Stats.run rs (fun () -> Tbl.add tbl ~key ~data) in
        loop (i + 1) tbl
      end else
        tbl
    in
    (rs, loop 0 (Tbl.create ~hash:(fun _ -> 1) 250_000))

  let worst_case_lookup samples tbl =
    let rs = Stats.create () in
    for i = 1 to 1_000_000 do
      let key = Strings.next samples in
      ignore (Stats.run rs (fun () -> Tbl.find tbl key));
      Strings.bash_cache samples;
    done;
    rs

  let memory_overhead samples size n =
    let rec loop i tbl =
      if i < n then
        loop (i + 1) (Tbl.add tbl ~key:(Strings.random samples) ~data:())
      else
        tbl
    in
    let tbl = loop 0 (Tbl.create size) in
    let words = float (Size.words tbl) in
    let (data, c) =
      Tbl.fold tbl ~init:(0, 0) ~f:(fun ~key ~data (w, c) ->
        (w + Size.words key + Size.words data, c + 1))
    in
    (words -. float data) /. float c
  ;;

  let compact () =
    let current_control = Gc.get () in
    Gc.set {current_control with Gc.Control.space_overhead = 5};
    Gc.compact ();
    Gc.set current_control
  ;;

  let trial tests =
    (* turn off compaction *)
    Gc.set
      { (Gc.get ()) with Gc.Control.
        max_overhead = 1_000_000; };
    let samples = Strings.create () in
    if List.mem tests `insert_and_lookup then begin
      let (rs, tbl) = insert samples 1 1_000_000 in
      Printf.printf "insert,";
      Stats.print rs;
      let rs = lookup samples tbl in
      Printf.printf "lookup,";
      Stats.print rs;
    end;
    if List.mem tests `insert_and_lookup_no_growth then begin
      Printf.printf "insert no growth,";
      let (rs, tbl) = insert samples 1_000_000 1_000_000 in
      Stats.print rs;
      Printf.printf "lookup no growth,";
      let rs = lookup samples tbl in
      Stats.print rs
    end;
    if List.mem tests `memory_overhead then begin
      Printf.printf "memory overhead,";
      Printf.printf "%f\n%!" (memory_overhead samples 1 10_000);
    end;
    if List.mem tests `memory_overhead_no_growth then begin
      Printf.printf "memory overhead,";
      Printf.printf "%f\n%!" (memory_overhead samples 10_000 10_000);
    end;
    if List.mem tests `worst_case then begin
      Printf.printf "worst case insert,";
      let (rs, tbl) = worst_case_insert samples in
      Stats.print rs;
      Printf.printf "worst case lookup,";
      let rs = worst_case_lookup samples tbl in
      Stats.print rs
    end
  ;;

  let run tests trials =
    for i = 1 to trials do
      trial tests
    done
end

module Arg = struct
  open Arg

  type tests =
  [ `insert_and_lookup
  | `insert_and_lookup_no_growth
  | `memory_overhead
  | `memory_overhead_no_growth
  | `worst_case ]
  with sexp

  type t =
  [ `Htree
  | `Fastcache
  | `Standard
  | `Super_htree
  | `Standard_map
  | `Fast_map ]

  let tbl = ref `Htree
  let tests = ref "(insert_and_lookup insert_and_lookup_no_growth memory_overhead memory_overhead_no_growth)"
  let trials = ref 1
  let args =
    Arg.align
      [ ("-htree", Unit (fun () -> tbl := `Htree),
        " run tests on the htree");
        ("-fastcache", Unit (fun () -> tbl := `Fastcache),
        " run tests on the fastcache");
        ("-standard", Unit (fun () -> tbl := `Standard),
        " run tests on the standard");
        ("-separate-chaining", Unit (fun () -> tbl := `Separate_chaining),
        " run tests on the separate chaining hashtbl");
        ("-core-extended-table", Unit (fun () -> tbl := `Super_htree),
        " run tests on the super htree aka Core_extended.Table_new");
        ("-standard-map", Unit (fun () -> tbl := `Standard_map),
        " run tests on the standard map (from the caml stdlib)");
        ("-fast-map", Unit (fun () -> tbl := `Fast_map),
        " run tests on the optimized map in core_extended");
        ("-tests", Set_string tests,
        sprintf " list of tests to run %s" !tests);
        ("-trials", Set_int trials,
        "<i> number of times to run the specified tests") ]

  let parse () =
    Arg.parse args (fun a -> raise (Bad a))
      (sprintf "usage %s: <args>\n\nWarning! The worst case test does not work with the standard hashtbl, it will give wrong results" Sys.argv.(0));
    (!tbl, List.t_of_sexp tests_of_sexp (Sexp.of_string !tests),
     !trials)
end


let () =
  let (tbl, tests, trials) = Arg.parse () in
  match tbl with
  | `Htree ->
      let module M = Make (Of_table_new (Htree)) in
      M.run tests trials
  | `Super_htree ->
      let module M = Make (Of_table_new (Table_new)) in
      M.run tests trials
  | `Fastcache ->
      let module M = Make (Of_table_new (Hashtbl_fastcache)) in
      M.run tests trials
  | `Standard ->
      let module M = Make (Std_hashtbl) in
      M.run tests trials
  | `Separate_chaining ->
      let module M = Make (Of_table_new (Hashtbl_separate_chaining)) in
      M.run tests trials
  | `Standard_map ->
      let module M = Make (Of_map (Core.Std.Map)) in
      M.run tests trials
  | `Fast_map -> ()
  (* | `Fast_map ->
   *     let module M = Make (Of_map (Core_extended.Fast_map)) in
   *     M.run tests trials *)
