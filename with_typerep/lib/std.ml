
open Typerep_experimental.Std

include (Core.Std : module type of Core.Std
  with module Month   := Core.Std.Month
   and module Date    := Core.Std.Date
   and module Time    := Core.Std.Time
)

open Core.Std

module Serializable_of_typestructable(T : Typestructable.S0) = struct
  type t = Tagged.t
  include (T : Typestructable.S0 with type t := T.t)
  module B = Binrep.Tagged.Make_binable(T)
  include B
  include Sexprep.Tagged.Make_sexpable(T)
  let binable_b =
    let module B' = struct
      type nonrec t = t
      include B
    end in
    (module B' : Binable.S with type t = t)
  let t_of_string = Binable.of_string binable_b
  let string_of_t = Binable.to_string binable_b
  let t_of_bigstring = Binable.of_bigstring binable_b
  let bigstring_of_t = Binable.to_bigstring binable_b
end

(* A type with [Binable] and [Sexpable]. *)
module type With_bin_io_and_sexp = sig
  type t

  include Binable.S with type t := t
  include Sexpable.S with type t := t
end

module type With_typerep_and_typestruct = sig
  type t

  include Typerepable.S0 with type t := t
  include Typestructable.S0 with type t := t
end

module Extending_with_typerep_test : sig

  (* entry point of tests *)
  val run :
    ?skip_sexp_str:bool
    -> (module With_bin_io_and_sexp with type t = 'a)
    -> (module With_typerep_and_typestruct with type t = 'a)
    -> 'a list
    -> unit

  (* Checks that the [Binable] and [Sexpable] implementations generated from the typerep
     are equivalent with the auto-generated ones. *)
  val run_with_typerep :
    (module With_bin_io_and_sexp with type t = 'a)
    -> (module Typerepable.S0 with type t = 'a)
    -> 'a list
    -> unit

  (* Checks that the [Binable] and [Sexpable] implementations generated from the
     typestruct are equivalent with the auto-generated ones. *)
  val run_with_typestruct :
    skip_sexp_str:bool
    -> (module With_bin_io_and_sexp with type t = 'a)
    -> (module Typestructable.S0 with type t = 'a)
    -> 'a list
    -> unit
end = struct

  let run_with_typerep (type a) camlp4 rep (items : a list) =
    (* Make the modules we need. *)
    let module R = (val rep : Typerepable.S0 with type t = a) in
    let module M = struct
      type t = a
      include Binrep.Make_binable(R)
      include Sexprep.Make_sexpable(R)
    end in
    let module P = (val camlp4 : With_bin_io_and_sexp with type t = a) in
    (* Project the modules to [Binable]. *)
    let binable_m = (module M : Binable.S with type t = a) in
    let binable_p = (module P : Binable.S with type t = a) in
    (* Check that the binary encodings are identical. *)
    List.iter items
      ~f:(fun item ->
        let bin_from_p = Binable.to_string binable_p item in
        let bin_from_m =
          try Binable.to_string binable_m item
          with e ->
            Printf.printf "exn while bin_io rep serializing (1) using %s\nbuffer:%S\n%s\n%!"
              (Sexp.to_string_hum (Type_struct.sexp_of_typerep R.typerep_of_t))
              bin_from_p
              (Exn.to_string e);
            raise e
        in
        assert (String.equal bin_from_p bin_from_m);
        let str = bin_from_m in (* they are the same *)
        (* deserialize and reserialize again *)
        let item_p = Binable.of_string binable_p str in
        let item_m =
          try Binable.of_string binable_m str
          with e ->
            Printf.printf "exn while bin_io rep unserializing using %s\nbuffer:%S\n%s\n%!"
              (Sexp.to_string_hum (Type_struct.sexp_of_typerep R.typerep_of_t))
              str
              (Exn.to_string e);
            raise e
        in
        let bin_from_p = Binable.to_string binable_p item_p in
        let bin_from_m =
          try Binable.to_string binable_m item_m
          with e ->
            Printf.printf "exn while bin_io rep serializing (2) using %s\nbuffer:%S\n%s\n%!"
              (Sexp.to_string_hum (Type_struct.sexp_of_typerep R.typerep_of_t))
              bin_from_p
              (Exn.to_string e);
            raise e
        in
        (* and recheck *)
        assert (String.equal bin_from_p bin_from_m);
        (* inverting the values *)
        let bin_from_p = Binable.to_string binable_p item_m in
        let bin_from_m =
          try Binable.to_string binable_m item_p
          with e ->
            Printf.printf "exn while bin_io rep serializing (3) using %s\nbuffer:%S\n%s\n%!"
              (Sexp.to_string_hum (Type_struct.sexp_of_typerep R.typerep_of_t))
              bin_from_p
              (Exn.to_string e);
            raise e
        in
        (* and recheck *)
        assert (String.equal bin_from_p bin_from_m);
      );
    (* Check that the sexp encodings are identical. *)
    List.iter items
      ~f:(fun item ->
        let sexp_from_m = M.sexp_of_t item in
        let sexp_from_p = P.sexp_of_t item in
        assert (String.equal (Sexp.to_string sexp_from_m) (Sexp.to_string sexp_from_p));
        (* deserialize and reserialize again *)
        let item_m = M.t_of_sexp sexp_from_p in
        let item_p = P.t_of_sexp sexp_from_m in
        let sexp_from_m = M.sexp_of_t item_m in
        let sexp_from_p = P.sexp_of_t item_p in
        (* and recheck *)
        assert (String.equal (Sexp.to_string sexp_from_m) (Sexp.to_string sexp_from_p));
      );
  ;;

  let run_with_typestruct (type a) ~skip_sexp_str camlp4 str (items : a list) =
    (* Make the modules we need. *)
    let module R = (val str : Typestructable.S0 with type t = a) in
    let module M = Serializable_of_typestructable(R) in
    let module P = (val camlp4 : With_bin_io_and_sexp with type t = a) in
    (* Project the modules to [Binable]. *)
    let binable_m = (module M : Binable.S with type t = M.t) in
    let binable_p = (module P : Binable.S with type t = a) in
    List.iter items
      ~f:(fun item ->
        let bin_from_p = Binable.to_string binable_p item in
        let item_m =
          try Binable.of_string binable_m bin_from_p
          with e ->
            Printf.printf "exn while bin_io struct unserializing using %s\nbuffer:%S\n%s\n%!"
              (Sexp.to_string_hum (Type_struct.sexp_of_t R.typestruct_of_t))
              bin_from_p
              (Exn.to_string e);
            raise e
        in
        let bin_from_m =
          try Binable.to_string binable_m item_m
          with e ->
            Printf.printf "exn while bin_io struct serializing using %s\nbuffer:%S\n%s\n%!"
              (Sexp.to_string_hum (Type_struct.sexp_of_t R.typestruct_of_t))
              bin_from_p
              (Exn.to_string e);
            raise e
        in
        assert (String.equal bin_from_p bin_from_m);

        if skip_sexp_str then () else begin
          let sexp_m = M.sexp_of_t item_m in
          try
            let item_p = P.t_of_sexp sexp_m in
            let bin_from_p2 = Binable.to_string binable_p item_p in
            assert (String.equal bin_from_p bin_from_p2)
          with
          | exn -> begin
            Printf.eprintf "exn: %s\n" (Exn.to_string exn);
            Printf.eprintf "M.sexp_of_t: %s\n" (Sexp.to_string_hum sexp_m);
            Printf.eprintf "P.sexp_of_t: %s\n" (Sexp.to_string_hum (P.sexp_of_t item));
            Printf.eprintf "%!";
            assert false
          end;
        end
      );
  ;;

  let run (type a) ?(skip_sexp_str=false) camlp4 generic (items : a list) =
    let module G = (val generic : With_typerep_and_typestruct with type t = a) in
    let rep = (module G : Typerepable.S0 with type t = a) in
    let str = (module G : Typestructable.S0 with type t = a) in
    run_with_typerep camlp4 rep items;
    run_with_typestruct ~skip_sexp_str camlp4 str items;
  ;;
end

module Test = Extending_with_typerep_test


module Month = struct
  include (Month : (module type of Month with module Stable := Month.Stable))
  module Stable = struct
    module V1 = struct
      type t = Month.Stable.V1.t =
        | Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
      with typerep(abstract)
      include (Month.Stable.V1 : (module type of Month.Stable.V1 with type t := t))
      module T = struct
        type t =
        | Jan
        | Feb
        | Mar
        | Apr
        | May
        | Jun
        | Jul
        | Aug
        | Sep
        | Oct
        | Nov
        | Dec
        with typerep
      end
      include Typestructable.Of_typerepable(T)
    end
    let () = Customrep.register0 (module V1 : Customrep_intf.S0)
  end
  TEST_UNIT =
    let months =
      (* Nobody likes Oct. *)
      (* Especially after hurricane Sandy hit. Nice bit of premonition by ascvortov *)
      [ Month.Jan; Feb; Mar; Apr; May; Jun; Jul; Aug; Sep; Nov; Dec ]
    in
    Test.run
      (module Month.Stable.V1 : With_bin_io_and_sexp  with type t = Month.Stable.V1.t)
      (module Stable.V1 : With_typerep_and_typestruct with type t = Month.Stable.V1.t)
      (* List.map ~f:Month.Stable.V1.of_current *) months;
  ;;
end

module Date = struct
  include (Date : (module type of Date with module Stable := Date.Stable))
  module Stable = struct
    module V1 = struct
      type t = Date.Stable.V1.t
      with typerep(abstract)
      include (Date.Stable.V1 : (module type of Date.Stable.V1 with type t := t))
      module T = struct
        type t = { y : int ; m : Month.Stable.V1.t ; d : int } with typerep
      end
      include Typestructable.Of_typerepable(T)
    end
    let () = Customrep.register0 (module V1 : Customrep_intf.S0)
  end
  TEST_UNIT =
    let dates =
      (List.map ~f:Date.of_string
         [ "1970-01-01"; "1990-07-15"; "1996-02-29"; "2012-08-14" ])
    in
    Test.run ~skip_sexp_str:true
      (module Date.Stable.V1 : With_bin_io_and_sexp   with type t = Date.Stable.V1.t)
      (module Stable.V1 : With_typerep_and_typestruct with type t = Date.Stable.V1.t)
      (* List.map ~f:Date.Stable.V1.of_current *) dates;
  ;;
end

module Time = struct
  include (Time : (module type of Time with module Stable := Time.Stable))
  module Stable = struct
    module V1 = struct
      type t = Time.Stable.V1.t with typerep(abstract)
      include (Time.Stable.V1 : (module type of Time.Stable.V1 with type t := t))
      module T = struct
        type t = float with typerep
      end
      include Typestructable.Of_typerepable(T)
    end
    let () = Customrep.register0 (module V1 : Customrep_intf.S0)
  end
  TEST_UNIT =
    let times =
      (List.map ~f:Time.of_string
         [ "2012-08-14 15:23:40" ])
    in
    Test.run ~skip_sexp_str:true
      (module Time.Stable.V1 : With_bin_io_and_sexp   with type t = Time.Stable.V1.t)
      (module Stable.V1 : With_typerep_and_typestruct with type t = Time.Stable.V1.t)
      (* List.map ~f:Time.Stable.V1.of_current *) times;
  ;;
end
