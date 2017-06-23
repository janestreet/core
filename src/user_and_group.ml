open! Import

module Stable = struct
  module V1 = struct
    module T = struct
      type t = { user : string; group : string } [@@deriving fields, bin_io, compare, hash]

      let to_string t = sprintf "%s:%s" t.group t.user

      let of_string str =
        let user, group = String.lsplit2_exn str ~on:':' in
        if String.contains group ':' then
          failwithf ("User_and_group.of_string: malformed [%s]:"
                     ^^ " unix group names may not contain colons") str ();
        { user; group }
    end
    include T
    include Sexpable.Stable.Of_stringable.V1(T)
  end
end

module T' = struct
  include Stable.V1
  let module_name = "Core.User_and_group"
end
include T'
include Identifiable.Make (T')

let%test _ = equal { user = "foo"; group = "bar" } (of_string "foo:bar")

let create = Fields.create

let for_this_process () =
  let user = Unix.getlogin () in
  let gid = Unix.getgid () in
  match Core_unix.Group.getbygid gid with
  | None -> Or_error.error "Couldn't get group" (`gid gid) [%sexp_of: [ `gid of int ]]
  | Some group -> Ok (create ~user ~group:group.Core_unix.Group.name)

let for_this_process_exn () = Or_error.ok_exn (for_this_process ())
