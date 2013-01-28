(** A type for making staging explicit in the type of a function.  For example, you might
    want to have a function that creates a function for allocating unique identifiers.
    Rather than using the type:

    {[
    val make_id_allocator : unit -> unit -> int
    ]}

    you would have

    {[
    val make_id_allocator : unit -> (unit -> int) Staged.t
    ]}

    Such a function could be defined as follows:

    {[
    let make_id_allocator () =
      let ctr = ref 0 in
      stage (fun () -> incr ctr; !ctr)
    ]}

    and could be invoked as follows:

    {[
    let (id1,id2) =
      let alloc = unstage (make_id_allocator ()) in
      (alloc (), alloc ())
    ]}

    both stage and unstage functions are available in Common.

    (Note that in many cases, including perhaps the one above, it's preferable to create a
    custom type rather than use Staged.)
*)

type +'a t

val stage   : 'a -> 'a t
val unstage : 'a t -> 'a

