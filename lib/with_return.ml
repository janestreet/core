(* belongs in Common, but moved here to avoid circular dependencies *)

type 'a return = { return : 'b. 'a -> 'b }

let with_return f =
  let module M = struct
    (* Raised to indicate ~return was called.  Local so that the exception is tied to a
       particular call of [with_return]. *)
    exception Return
  end
  in
  let r = ref None in                   (* stores the return value *)
  let return = {                        (* closure passed to f *)
    return = (fun x ->
      r := Some x;
      raise M.Return);
  }
  in
  try
    let rval = f return in
    begin match !r with
    | None -> rval
    | Some _ -> failwith "with_return exited normally despite return being called"
    end
  with M.Return ->                      (* allows other exceptions through *)
    match !r with
    | None -> assert false
    | Some x -> x
