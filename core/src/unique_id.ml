open! Import
open Unique_id_intf

module type Id = Id

module type Int_base = sig
  include Id_without_create with type t = int

  val zero : t
end

module Int_base : Int_base = Int

module type Int63_base = sig
  include Id_without_create with type t = Int63.t

  val zero : t
  val succ : t -> t
end

module Int63_base : Int63_base = Int63

module Ref = struct
  (* See the comment on [Atomic.Int] below. *)
  module Int () = struct
    include (
      Int_base :
        module type of struct
          include Int_base
        end
        with module Map := Int_base.Map
         and module Set := Int_base.Set)

    module Map = Map.Make_binable_using_comparator (Int_base)
    module Set = Set.Make_binable_using_comparator (Int_base)

    let current = ref zero

    (* Unlike [Int63.create], here our "succ" operation (`x + 1`) never allocates and
       cannot context-switch. But we keep the same loop structure in place for
       consistency, even though this loop should never be executed more than once. *)
    let rec create () =
      let x = !current in
      let new_x = x + 1 in
      if phys_equal !current x
      then (
        current := new_x;
        x)
      else create ()
    ;;

    module For_testing = struct
      let reset_counter () = current := zero
    end
  end

  module Int63 () = struct
    include (
      Int63_base :
        module type of struct
          include Int63_base
        end
        with module Map := Int63_base.Map
         and module Set := Int63_base.Set)

    module Map = Map.Make_binable_using_comparator (Int63_base)
    module Set = Set.Make_binable_using_comparator (Int63_base)

    let current = ref zero

    (* Only "succ" can cause a context-switch that might lead to a race. Thus we have to
       check whether the contents of the cell remained unchanged across this call. The
       subsequent comparison, dereferencing and assignment cannot cause context switches.
       If the contents of the cell had changed, we will have to try again to obtain a
       unique id. This is essentially like a spin-lock and is virtually guaranteed to
       succeed quickly. *)
    let rec create () =
      let x = !current in
      let new_x = succ x in
      if phys_equal !current x
      then (
        current := new_x;
        x)
      else create ()
    ;;

    module For_testing = struct
      let reset_counter () = current := zero
    end
  end
end

module Atomic = struct
  module Int () = struct
    include (
      Int_base :
        module type of struct
          include Int_base
        end
        with module Map := Int_base.Map
         and module Set := Int_base.Set)

    module Map = Map.Make_binable_using_comparator (Int_base)
    module Set = Set.Make_binable_using_comparator (Int_base)

    let current = Atomic.make ~padded:true zero
    let create () = Atomic.fetch_and_add current 1

    module For_testing = struct
      let reset_counter () = Atomic.set current zero
    end
  end

  module type Id_int63 = Id with type t = private Int63.t

  module Int63_emul () : Id_int63 = struct
    include (
      Int63_base :
        module type of struct
          include Int63_base
        end
        with module Map := Int63_base.Map
         and module Set := Int63_base.Set)

    module Map = Map.Make_binable_using_comparator (Int63_base)
    module Set = Set.Make_binable_using_comparator (Int63_base)

    let current = Atomic.make zero

    let create =
      let succ = [%eta1 succ] in
      fun () -> Atomic.get_and_update current ~pure_f:succ
    ;;

    module For_testing = struct
      let reset_counter () = Atomic.set current zero
    end
  end

  let int63 () : (module Id_int63) =
    match Int63.Private.repr with
    | Int64 -> (module Int63_emul () : Id_int63)
    | Int ->
      (* The [Int] case uses the [Int]-based counter (faster [Atomic.fetch_and_add] on a
         tagged-int counter), but overrides [bin_shape_t] so that the [bin_digest] of
         [Atomic.Int63] matches the [Int63_emul] case. This keeps [bin_digest]s stable
         across native and JavaScript builds. This mirrors the pattern used by
         [Core.Int63] itself, which delegates serialization to [Int] on native but reports
         the [int63] shape. *)
      let module Source = Int () in
      let module M = struct
        include Source

        let bin_shape_t = Bin_prot.Shape.bin_shape_int63

        module Stable = struct
          module V1 = struct
            include Source.Stable.V1

            let bin_shape_t = Bin_prot.Shape.bin_shape_int63
          end
        end
      end
      in
      (module M : Id_int63)
  ;;

  module Int63 () : Id_int63 = struct
    include (val int63 ())
  end
end

include Ref
