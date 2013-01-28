open Core.Std
open OUnit
open Unix

let with_large_file f =
  let filename = Filename.temp_file "large" "test" in
  protect ~f:(fun () ->
    ignore (system (sprintf
      "dd if=/dev/zero of=%s bs=1 count=1 seek=$(echo '2 ^ 32 + 1' | bc -l) > /dev/null 2>/dev/null"
        filename));
    ignore (f filename))
    ~finally:(fun () -> Sys.remove filename);
  true

let test =
  "core_unix" >:::
    [ "stat" >::
      (fun () ->
        "stat-large" @? with_large_file (fun fn -> stat fn);
        "lstat-large" @? with_large_file (fun fn -> lstat fn));
    ]
