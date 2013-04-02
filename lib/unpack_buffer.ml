open Std_internal

let debug = ref false

module Unpack_one = struct
  type ('value, 'partial_unpack) t =
    ?partial_unpack:'partial_unpack
    -> ?pos:int
    -> ?len:int
    -> Bigstring.t
    -> [ `Ok of 'value * int
       | `Not_enough_data of 'partial_unpack * int
       | `Invalid_data of Error.t
       ]

  let map t ~f =
    fun ?partial_unpack ?pos ?len buf ->
      match t ?partial_unpack ?pos ?len buf with
      | `Invalid_data _ | `Not_enough_data _ as x -> x
      | `Ok (a, pos) -> `Ok (f a, pos)
  ;;

  let create unpack_one =
    fun ?partial_unpack ?pos ?len buf ->
      let (pos, len) =
        Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(Bigstring.length buf)
      in
      unpack_one ?partial_unpack buf ~pos ~len
  ;;

  let create_bin_prot bin_prot_reader =
    let header_length = 8 in
    let not_enough_data = `Not_enough_data ((), 0) in
    let pos_ref = ref 0 in
    let invalid_data message a sexp_of_a =
      `Invalid_data (Error.create message a sexp_of_a)
    in
    let read bin_reader buf ~pos ~len =
      pos_ref := pos;
      let result = bin_reader buf ~pos_ref in
      if !pos_ref <> pos + len then
        invalid_data "pos_ref <> pos + len" (!pos_ref, pos, len)
          (<:sexp_of< int * int * int >>)
      else
        `Ok result
    in
    create
      (fun ?partial_unpack:_ buf ~pos ~len ->
        if header_length > len then
          not_enough_data
        else begin
          match read Bin_prot.Read_ml.bin_read_int_64bit buf ~pos ~len:header_length with
          | `Invalid_data _ as x -> x
          | `Ok element_length ->
            if element_length < 0 then
              invalid_data "negative element length %d" element_length <:sexp_of< int >>
            else begin
              if element_length > len - header_length then
                not_enough_data
              else begin
                match
                  read bin_prot_reader.Bin_prot.Type_class.read
                    buf ~pos:(pos + header_length) ~len:element_length
                with
                | `Invalid_data _ as x -> x
                | `Ok result -> `Ok (result, header_length + element_length)
              end
            end
        end)
  ;;

  type partial_sexp = (Bigstring.t, Sexp.t) Sexp.parse_fun sexp_opaque with sexp_of

  let sexp =
    let module Parse_pos = Sexp.Parse_pos in
    let partial_unpack_init ~pos ~len buf =
      Sexp.parse_bigstring buf ~len ~parse_pos:(Parse_pos.create ~buf_pos:pos ())
    in
    create
      (fun ?(partial_unpack = partial_unpack_init) buf ~pos ~len ->
        try
          begin match partial_unpack ~pos ~len buf with
          | Sexp.Cont (_state, k) -> `Not_enough_data (k, len)
          | Sexp.Done (sexp, parse_pos) -> `Ok (sexp, parse_pos.Parse_pos.buf_pos - pos)
          end
        with exn -> `Invalid_data (Error.of_exn exn))
  ;;
end

type ('a, 'b) alive =
  { mutable partial_unpack : 'b option;
    unpack_one : ('a, 'b) Unpack_one.t sexp_opaque;
    (* [buf] holds unconsumed chars *)
    mutable buf : Bigstring.t;
    (* [pos] is the start of unconsumed data in [buf] *)
    mutable pos : int;
    (* [len] is the length of unconsumed data in [buf] *)
    mutable len : int;
  }
  with sexp_of

type ('a, 'b) state =
| Alive of ('a, 'b) alive
| Dead of Error.t
with sexp_of

type ('a, 'b) t =
  { mutable state : ('a, 'b) state;
  }
with sexp_of

let sexp_of_any _ = Sexp.Atom "<VALUE>"

let to_sexp_ignore t = sexp_of_t sexp_of_any sexp_of_any t

let invariant t =
  try
    match t.state with
    | Dead _ -> ()
    | Alive t ->
      assert (t.pos >= 0);
      assert (t.len >= 0);
      if t.len = 0 then assert (t.pos = 0);
      assert (t.pos + t.len <= Bigstring.length t.buf);
  with exn ->
    failwiths "invariant failed" (exn, to_sexp_ignore t) <:sexp_of< exn * Sexp.t >>
;;

let create ?partial_unpack unpack_one =
  { state =
      Alive { partial_unpack;
               unpack_one;
               buf = Bigstring.create 1;
               pos = 0;
               len = 0;
             };
  }
;;

let create_bin_prot bin_prot_reader = create (Unpack_one.create_bin_prot bin_prot_reader)

let is_empty t =
  match t.state with
  | Dead error -> Error error
  | Alive t -> Ok (is_none t.partial_unpack && t.len = 0)
;;

let is_empty_exn t = ok_exn (is_empty t)

let is_available t len =
  let input_start = t.pos + t.len in
  let available = Bigstring.length t.buf - input_start in
  available >= len
;;

let ensure_available t len =
  if not (is_available t len) then begin
    (* Grow the buffer, and shift the unconsumed bytes to the front. *)
    let new_buf = Bigstring.create (max (t.len + len) (2 * Bigstring.length t.buf)) in
    Bigstring.blit ~src:t.buf ~src_pos:t.pos ~src_len:t.len ~dst:new_buf ();
    t.pos <- 0;
    t.buf <- new_buf;
    assert (is_available t len);
  end;
;;

let feed_gen buf_length (blit_buf_to_bigstring : (_, _) Bigstring.blit)
    ?pos ?len t buf =
  if !debug then invariant t;
  match t.state with
  | Dead e -> Error e
  | Alive t ->
    let (src_pos, src_len) =
      Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(buf_length buf)
    in
    ensure_available t src_len;
    blit_buf_to_bigstring
      ~src:buf ~src_pos ~src_len
      ~dst:t.buf ~dst_pos:(t.pos + t.len) ();
    t.len <- t.len + src_len;
    Ok ();
;;

let feed ?pos ?len t buf =
  feed_gen Bigstring.length Bigstring.blit                  ?pos ?len t buf
;;

let feed_string ?pos ?len t buf =
  feed_gen    String.length Bigstring.blit_string_bigstring ?pos ?len t buf
;;

let unpack t =
  if !debug then invariant t;
  match t.state with
  | Dead e -> Error e
  | Alive alive ->
    let result = Queue.create () in
    let error e =
      t.state <- Dead e;
      (* If we *have* unpacked values, we first want to return them,
         and then on the next call we will return the error (because [t.state = Dead]) *)
      if Queue.is_empty result then Error e else Ok result
    in
    let t = alive in
    let consume ~num_bytes =
      t.pos <- t.pos + num_bytes;
      t.len <- t.len - num_bytes;
    in
    let rec loop () =
      if t.len = 0 then begin
        t.pos <- 0;
        Ok result;
      end else begin
        match
          Result.try_with (fun () ->
            t.unpack_one t.buf ~pos:t.pos ~len:t.len ?partial_unpack:t.partial_unpack)
        with
        | Error exn -> error (Error.create "unpack error" exn <:sexp_of< Exn.t >>)
        | Ok unpack_result ->
          match unpack_result with
          | `Invalid_data e -> error (Error.tag e "invalid data")
          | `Ok (one, num_bytes) ->
            (* In order to get a value we either need to consume some bytes or have
               partially unpacked data, otherwise it is a bug in [unpack_one].  The case
               of [num_bytes = 0] comes up when parsing sexp atoms where we don't know
               where atom ends until we hit parenthesis, e.g. "abc(". *)
            if num_bytes < 0 || num_bytes > t.len then
              error (Error.create "unpack consumed invalid amount" num_bytes
                       (<:sexp_of< int >>))
            else if num_bytes = 0 && Option.is_none t.partial_unpack then
              error (Error.of_string "\
unpack returned a value but consumed 0 bytes without partially unpacked data")
            else begin
              consume ~num_bytes;
              t.partial_unpack <- None;
              Queue.enqueue result one;
              loop ();
            end;
          | `Not_enough_data (partial_unpack, num_bytes) ->
            (* Partial unpacking need not have consumed any bytes, and cannot have
               consumed more bytes than were available. *)
            if num_bytes < 0 || num_bytes > t.len then
              error (Error.create "partial unpack consumed invalid amount" num_bytes
                       (<:sexp_of< int >>))
            else begin
              consume ~num_bytes;
              t.partial_unpack <- Some partial_unpack;
              (* Put unconsumed bytes at the front.  We assume that unpacking is
                 deterministic, which ensures that every input byte is shifted at most
                 once.  Once a byte has been shifted, it will remain where it is until it
                 is consumed. *)
              if t.len > 0 then
                Bigstring.blit ~src:t.buf ~src_pos:t.pos ~src_len:t.len ~dst:t.buf ();
              t.pos <- 0;
              Ok result;
            end
      end
    in
    loop ()
;;

TEST_MODULE "unpack-buffer" = struct

  module type Value = sig
    type t with sexp_of
    include Equal.S with type t := t
    val pack : t list -> string
    type partial_unpack with sexp_of
    val unpack_one : (t, partial_unpack) Unpack_one.t
  end

  let test (type value) (v : (module Value with type t = value)) values =
    let module V = (val v) in
    let input = Bigstring.of_string (V.pack values) in
    let input_size = Bigstring.length input in
    for chunk_size = 1 to input_size do
      let t = create V.unpack_one in
      try
        assert (is_empty_exn t);
        let output = Queue.create () in
        let rec loop pos =
          if pos < input_size then begin
            let len = min chunk_size (input_size - pos) in
            assert (feed t input ~pos ~len = Ok ());
            assert (not (is_empty_exn t));
            let unpack_result = ok_exn (unpack t) in
            Queue.transfer ~src:unpack_result ~dst:output;
            loop (pos + len);
          end
        in
        loop 0;
        assert (is_empty_exn t);
        let output = Queue.to_list output in
        if not (List.equal ~equal:V.equal values output) then
          failwiths "mismatch" (values, output) <:sexp_of< V.t list * V.t list >>;
      with exn ->
        failwiths "failure"
          (exn, `chunk_size chunk_size, `input input, values, t)
          (<:sexp_of< (exn
                       * [ `chunk_size of int ]
                       * [ `input of Bigstring.t ]
                       * V.t list
                       * (V.t, V.partial_unpack) t )>>);
    done;
  ;;

  TEST_UNIT =
    debug := true;
    for value_size = 1 to 5 do
      let module Value = struct
        let pack ts = String.concat ts
        type partial_unpack = unit with sexp_of
        let unpack_one =
          Unpack_one.create
            (fun ?partial_unpack:_ buf ~pos ~len ->
              if len < value_size then
                `Not_enough_data ((), 0)
              else
                let string = String.create value_size in
                Bigstring.blit_bigstring_string ~src:buf ~src_pos:pos ~src_len:value_size
                  ~dst:string ();
                `Ok (string, value_size))
        include String
      end in
      let values =
        List.init 10 ~f:(fun i ->
          String.init value_size ~f:(fun j ->
            Char.of_int_exn ((i * value_size + j) land 0xFF)))
      in
      test (module Value) values
    done;
  ;;

  (* [Unpack_one.sexp] *)
  TEST_UNIT =
    let module Value = struct
      let pack ts = String.concat ~sep:" " (List.map ts ~f:Sexp.to_string)
      type partial_unpack = Unpack_one.partial_sexp with sexp_of
      let unpack_one = Unpack_one.sexp
      include Sexp
    end in
    let sexps =
      Sexp.(
        let e = Atom "" in
        let a = Atom "a" in
        let abc = Atom "abc" in
        [ e; a; abc
        ; List []
        ; List [ a ]
        ; List [ e; a ]
        ; List [ List [] ]
        ; List [ List []
               ; List [ a ]
               ; List [ a; abc ]
               ]])
    in
    let test sexps = test (module Value) sexps in
    let terminator = Sexp.List [] in (* used to ensure unparsing succeeds *)
    List.iter sexps ~f:(fun sexp ->
      test [ sexp; terminator ];
      test [ sexp; sexp; terminator ]);
    test sexps;
  ;;

  (* [Unpack_one.sexp] *)
  TEST_UNIT =
    debug := true;
    (* Error case. *)
    begin
      match Unpack_one.sexp ~pos:0 ~len:1 (Bigstring.of_string ")") with
      | `Invalid_data _ -> ()
      | `Ok _
      | `Not_enough_data _ -> assert false
    end;
    (* Simple, case where we parse a complete sexp in one pass:
       - starts in the middle of the buffer
       - doesn't consume the whole buffer *)
    begin
      match Unpack_one.sexp ~pos:1 ~len:9 (Bigstring.of_string ")(foo)(x y") with
      | `Ok (Sexp.List [Sexp.Atom "foo"], 5) -> ()
      | `Ok result ->
        Error.raise
          (Error.create "Unexpected result" result <:sexp_of<Sexp.t * int>>)
      | `Not_enough_data _
      | `Invalid_data _ -> assert false
    end;
    (* Partial sexp case, requries two passes to parse the sexp. *)
    begin
      match Unpack_one.sexp ~pos:6 ~len:4 (Bigstring.of_string ")(foo)(x y") with
      | `Not_enough_data (k, 4) ->
        begin
          match
            Unpack_one.sexp ~partial_unpack:k ~pos:0 ~len:3 (Bigstring.of_string " z)")
          with
          | `Ok (Sexp.List [Sexp.Atom "x"; Sexp.Atom "y"; Sexp.Atom "z"], 3) -> ()
          | `Ok _
          | `Not_enough_data _
          | `Invalid_data _ -> assert false
        end
      | `Not_enough_data (_, n) -> failwithf "Consumed %d bytes" n ()
      | `Ok result ->
        Error.raise
          (Error.create "Unexpected result" result <:sexp_of<Sexp.t * int>>)
      | `Invalid_data error -> Error.raise error
    end
  ;;

end
