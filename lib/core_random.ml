(* Unfortunately, because the standard library does not expose [Random.State.default],
   we have to construct our own.  We then build the [Random.int], [Random.bool] functions
   and friends using that default state in exactly the same way as the standard library.

   One other trickiness is that we need access to the unexposed [Random.State.assign]
   function, which accesses the unexposed state representation.  So, we copy the
   [State.repr] type definition and [assign] function to here from the standard library,
   and use [Obj.magic] to get access to the underlying implementation. *)
open Random

external random_seed: unit -> int = "caml_sys_random_seed";;

module State = struct
  include State

  type repr = { st : int array; mutable idx : int }

  let assign t1 t2 =
    let t1 = (Obj.magic t1 : repr) in
    let t2 = (Obj.magic t2 : repr) in
    Array.blit t2.st 0 t1.st 0 (Array.length t1.st);
    t1.idx <- t2.idx;
  ;;

  let full_init t seed = assign t (make seed)

  let default =
    (* We define Core's default random state as a copy of OCaml's default random state.
       This means that programs that use Core.Random will see the same sequence of random
       bits as if they had used OCaml.Random.  However, because [get_state] returns a
       copy, Core.Random and OCaml.Random are not using the same state.  If a program used
       both, each of them would go through the same sequence of random bits.  To avoid
       that, we reset OCaml's random state to a different seed, giving it a different
       sequence. *)
    let t = Random.get_state () in
    Random.init 137;
    t
  ;;

end

let default = State.default

let bits () = State.bits default
let int bound = State.int default bound
let int32 bound = State.int32 default bound
let nativeint bound = State.nativeint default bound
let int64 bound = State.int64 default bound
let float scale = State.float default scale
let bool () = State.bool default

let full_init seed = State.full_init default seed
let init seed = State.full_init default [| seed |]
let self_init () = init (random_seed())

let get_state () = `Consider_using_Random_State_default
let set_state s = State.assign default s
