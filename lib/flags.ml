open Std_internal

include Flags_intf

module Make (M : Make_arg) = struct
  include Int63

  let empty = zero

  let (+) a b = bit_or a b
  let (-) a b = bit_and a (bit_not b)

  let intersect = bit_and
  let complement = bit_not

  let do_intersect t1 t2 = bit_and t1 t2 <> zero
  let are_disjoint t1 t2 = bit_and t1 t2 =  zero

  let error message a sexp_of_a =
    let e = Error.create message a sexp_of_a in
    if M.should_print_error then
      eprintf "%s\n%!" (Sexp.to_string_hum (Error.sexp_of_t e));
    Error.raise e;
  ;;

  let () =
    if not M.allow_intersecting then begin
      let rec check l ac =
        match l with
        | [] -> ac
        | (flag, name) :: l ->
          let bad = List.filter l ~f:(fun (flag', _) -> do_intersect flag flag') in
          let ac = if List.is_empty bad then ac else (flag, name, bad) :: ac in
          check l ac
      in
      let bad = check M.known [] in
      if not (List.is_empty bad) then
        error "Flags.Make got intersecting flags" bad
          (<:sexp_of< (t * string * (t * string) list) list >>);
    end;
  ;;

  let sexp_of_t =
    (* We reverse [known] so that the fold below accumulates from right to left, giving a
       final list with elements in the same order as [known]. *)
    let known = List.rev M.known in
    fun t ->
      let leftover, flag_names =
        List.fold known ~init:(t, []) ~f:(fun (t, flag_names) (flag, flag_name) ->
          if bit_and t flag = flag
          then (t - flag, flag_name :: flag_names)
          else (t, flag_names))
      in
      if leftover = empty
      then <:sexp_of< string list >> flag_names
      else
        <:sexp_of< string list * [ `unrecognized_bits of string ] >>
          (flag_names, `unrecognized_bits (sprintf "0x%Lx" (to_int64 leftover)))
  ;;
end

(* Check that conflicting flags leads to an error. *)
TEST =
  Result.is_error
    (Result.try_with (fun () ->
      let module M =
            Make (struct
              let allow_intersecting = false
              let should_print_error = false
              let known = [ Int63.of_int 0x1, "";
                            Int63.of_int 0x1, "";
                          ]
            end)
      in
      ()))
;;

TEST_MODULE = struct
  let a = Int63.of_int 0x1
  let b = Int63.of_int 0x2
  let c = Int63.of_int 0xC

  include Make (struct
    let allow_intersecting = false
    let should_print_error = true
    let known =
      [ a, "a";
        b, "b";
        c, "c";
      ]
    ;;
  end)

  (* [sexp_of_t] *)
  TEST = Sexp.equal (sexp_of_t empty)   Sexp.(List [])
  TEST = Sexp.equal (sexp_of_t a)       Sexp.(List [ Atom "a" ])
  TEST = Sexp.equal (sexp_of_t c)       Sexp.(List [ Atom "c" ])
  TEST = Sexp.equal (sexp_of_t (a + b)) Sexp.(List [ Atom "a"; Atom "b" ])
  TEST_UNIT = ignore (sexp_of_t (of_int 0x10) : Sexp.t);

  (* +, - *)
  TEST = equal (a + a) a
  TEST = equal (a + b) (b + a)
  TEST = equal (a - a) empty
  TEST = equal ((a + b) - a) b

  (* [intersect] *)
  TEST = equal (intersect a a) a
  TEST = equal (intersect a b) empty
  TEST = equal (intersect (a + b) a) a

  (* [complement] *)
  TEST = equal (intersect (complement a) b) b

  (* [do_intersect] *)
  TEST = do_intersect a a
  TEST = not (do_intersect a b)
  TEST = do_intersect (a + b) a
  TEST = do_intersect (a + b) b
  TEST = not (do_intersect (a + b) c)

  (* [are_disjoint] *)
  TEST = are_disjoint a empty
  TEST = not (are_disjoint a a)
  TEST = are_disjoint a b
  TEST = are_disjoint b a
  TEST = not (are_disjoint (a + b) a)
  TEST = are_disjoint (a + b) c

end
