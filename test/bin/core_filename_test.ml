open OUnit;;
open Core

let concat_test p1 p2 res =
  (sprintf "%s ^/  %s" p1 p2) @?
  (if p1 ^/ p2 = res then
     true
   else begin
     eprintf "%s ^/ %s = %s (expected %s)"
       p1 p2 (p1 ^/ p2) res;
     false
   end)

let test =
  "core_filename" >:::
  [ "concat" >:: fun () ->
      (List.iter ~f:(fun (p1,p2,expected) -> concat_test p1 p2 expected)
         ["a/","/b","a/b";
          "a","./b","a/b";
          "a",".","a/.";
          "a","/","a/";
          "a/.","./","a/";
          "a///././/./.",".///././/././/b","a/b"
         ]
      )
  ]

