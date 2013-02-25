open Std_internal
open Int.Replace_polymorphic_compare

module Resizing_array : sig
  type t
  val create : initial_length:int -> t
  (* returns key for retrieving added value *)
  val add : t -> string -> int
  val get_exn : t -> int -> string
  val iteri : t -> f:(int -> string -> unit) -> unit

  val invariant : t -> unit
end = struct
  type t = {
    mutable array : string array;
    mutable length : int;
  }

  let dummy = "DUMMY"

  let create ~initial_length =
    assert (initial_length > 0);
    { array = Array.create ~len:initial_length dummy;
      length = 0;
    }
  ;;

  let add t s =
    let array_length = Array.length t.array in
    let init_length = t.length in
    if init_length = array_length then begin
      let new_array = Array.create ~len:(2 * array_length) dummy in
      Array.blit ~src:t.array ~src_pos:0 ~dst:new_array ~dst_pos:0 ~len:array_length;
      t.array <- new_array
    end;
    t.array.(init_length) <- s;
    t.length <- init_length + 1;
    init_length
  ;;

  let get_exn t i = Array.get t.array i

  let iteri t ~f =
    for i = 0 to t.length - 1 do
      f i t.array.(i)
    done
  ;;

  let invariant t =
    assert (0 <= t.length);
    assert (t.length <= Array.length t.array);
    Array.iteri t.array ~f:(fun i s ->
      if i < t.length then
        assert (not (phys_equal s dummy))
      else
        assert (phys_equal s dummy));
  ;;
end

module Make (X : sig
  val initial_table_size : int
end) = struct
  let () =
    if X.initial_table_size <= 0 then
      failwithf "non-positive initial table size for Interned_string.Make:  %d"
        X.initial_table_size ()

  module T : sig
    type t

    val compare : t -> t -> int
    val hash    : t -> int

    val of_string : string -> t
    val to_string : t -> string

    val invariant : unit -> unit
  end = struct
    type t = int

    let compare = Int.compare
    let hash    = Int.hash

    let ts_by_string = String.Table.create ~size:X.initial_table_size ()
    let strings_by_t = Resizing_array.create ~initial_length:X.initial_table_size

    let invariant () =
      Resizing_array.invariant strings_by_t;
      Hashtbl.iter ts_by_string ~f:(fun ~key ~data ->
        assert (String.equal (Resizing_array.get_exn strings_by_t data) key));
      Resizing_array.iteri strings_by_t ~f:(fun key data ->
        assert (Int.equal (Hashtbl.find_exn ts_by_string data) key));
    ;;

    let of_string s =
      match Hashtbl.find ts_by_string s with
      | Some t -> t
      | None ->
        let t = Resizing_array.add strings_by_t s in
        Hashtbl.set ts_by_string ~key:s ~data:t;
        t
    ;;

    let to_string t = Resizing_array.get_exn strings_by_t t
  end

  module T' = struct
    include T
    (* sexp and bin-io representations have to use strings *)
    include Sexpable.Of_stringable (T)
    include Binable .Of_stringable (T)
  end
  include T'
  (* hashing and comparing are sped up by using ints *)
  include Comparable.Make_binable (T')
  include Hashable  .Make_binable (T')

  let of_bigstring ?pos ?len bigstring =
    of_string (Bigstring.to_string ?pos ?len bigstring)
  ;;

  let pp formatter t = String.pp formatter (to_string t)
end

TEST_MODULE = struct
  module X = struct
    let initial_table_size = 128
  end

  TEST_UNIT =
    let module T = Make (X) in
    T.invariant ();

    let random_string () =
      let length = 1 + Random.int 10 in
      let chars =
        List.init length ~f:(fun _ -> Char.of_int_exn (Random.int 255))
      in
      String.of_char_list chars
    in

    let assert_equal s t =
      assert (String.equal s (T.to_string t));
      assert (T.equal t (T.of_string s));
    in

    let string_table = String.Table.create () in
    let t_table = T.Table.create () in

    for __ = 1 to 1000 do
      let s = random_string () in
      let t = T.of_string s in
      assert_equal s t;

      let change table key data ~data_equal =
        Hashtbl.change table key (function
          | None -> Some data
          | Some data' ->
            assert (data_equal data data');
            Some data)
      in

      change string_table s t ~data_equal:T.equal;
      change t_table t s ~data_equal:String.equal;

      Hashtbl.iter string_table ~f:(fun ~key:s ~data:t ->
        assert_equal s t;
        assert (String.equal (Hashtbl.find_exn t_table t) s));

      Hashtbl.iter t_table ~f:(fun ~key:t ~data:s ->
        assert_equal s t;
        assert (T.equal (Hashtbl.find_exn string_table s) t));

      T.invariant ();
    done
  ;;

  TEST_UNIT =
    let module T = Make (X) in
    let foo () = T.of_string "foo" in
    let t1 = foo () in
    let t2 = foo () in
    let t3 = T.of_string "bar" in
    let t4 = foo () in
    assert (T.equal t1 t2);
    assert (T.equal t1 t4);
    assert (T.equal t2 t4);
    assert (not (List.exists ~f:(T.equal t3) [t1; t2; t4]));
    T.invariant ()
  ;;
end
