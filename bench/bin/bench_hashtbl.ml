open Core

module Bench = Core_bench.Bench
module Test = Bench.Test

module My_hashtbl = struct
  type ('k, 'v) t =
    { hash : ('k -> int)
    ; array : 'v option Array.t
    }

  let create ~size ~hash =
    let hash k = hash k mod size in
    { hash
    ; array = Array.create ~len:size None
    }

  let replace t ~key ~data =
    t.array.(t.hash key) <- Some data

  let find t key = t.array.(t.hash key)
end

module My_hashable = struct
  module Make(M:Hashtbl.Key) = struct
    module Table = struct
      include My_hashtbl

      let create ?(size=128) () =
        create ~size ~hash:M.hash
    end
  end
end

module Hashtbl_ = My_hashtbl
module Hashable_ = My_hashable

let gen_test_int_replace_and_find n tbl =
  let replace () =
    for i = 0 to n - 1 do
      Hashtbl.set tbl ~key:i ~data:i
    done
  in
  let find () =
    for i = 0 to n - 1 do
      let (_ : int option) = Hashtbl.find tbl i in ()
    done
  in
  replace, find

let () =
  let n = 1_000_000 in
  let int_tbl_replace1, int_tbl_find1 =
    let module I = Hashable.Make(struct
                     include Int
                     let hash x = Caml.Hashtbl.hash x
                   end)
    in
    gen_test_int_replace_and_find n (I.Table.create ~size:(2*n) ())
  in
  let int_tbl_replace2, int_tbl_find2 =
    let module I = Hashable.Make(struct
                     include Int
                     let hash x = Core.Hashtbl_intf.Hashable.hash x
                   end)
    in
    gen_test_int_replace_and_find n (I.Table.create ~size:(2*n) ())
  in
  let caml_hashtbl_hash () =
    for i = 0 to n - 1 do
      let (_ : int) = Caml.Hashtbl.hash i in ()
    done
  in
  let jst_hashtbl_hash () =
    for i = 0 to n - 1 do
      let (_ : int) = Core.Hashtbl_intf.Hashable.hash i in ()
    done
  in
  Bench.bench
    [ Test.create ~name:"Int-replace1"              int_tbl_replace1
    ; Test.create ~name:"Int-replace2"              int_tbl_replace2
    ; Test.create ~name:"Int-find1"                 int_tbl_find1
    ; Test.create ~name:"Int-find2"                 int_tbl_find2
    ; Test.create ~name:"Caml-hashtbl-hash"         caml_hashtbl_hash
    ; Test.create ~name:"Jst-hashtbl-hash"          jst_hashtbl_hash
    ]
